package bytepack

/*
 * Copyright (C) 2024 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import scala.deriving.*
import scala.compiletime.*
import java.nio.ByteBuffer

import reflect.Selectable.reflectiveSelectable

export enumextensions.EnumMirror

object Pack:
  given Pack[Byte] with
    def pack(i: Byte, b: java.nio.ByteBuffer) = b.put(i)
    def size = 1
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractByte(b, index)

  given Pack[Short] with
    def pack(i: Short, b: java.nio.ByteBuffer) = b.putShort(i)
    def size = 2
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractShort(b, index)

  given Pack[Int] with
    def pack(i: Int, b: java.nio.ByteBuffer) = b.putInt(i)
    def size = 4
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractInt(b, index)

  given Pack[Long] with
    def pack(i: Long, b: java.nio.ByteBuffer) = b.putLong(i)
    def size = 8
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractLong(b, index)

  given Pack[Float] with
    def pack(i: Float, b: java.nio.ByteBuffer) = b.putFloat(i)
    def size = 4
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractFloat(b, index)

  given Pack[Double] with
    def pack(i: Double, b: java.nio.ByteBuffer) = b.putDouble(i)
    def size = 8
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractDouble(b, index)

  given [T](using mirror: EnumMirror[T], sm: Mirror.SumOf[T]): Pack[T] with
    def pack(e: T, b: ByteBuffer): Unit = b.put(sm.ordinal(e).toByte)
    def size: Int = 1
    def unpack(index: Int, b: IArray[Byte]): T = mirror.fromOrdinal(b(index).toInt).get

  given [T](using mirror: EnumMirror[T], sm: Mirror.SumOf[T]): Pack[Option[T]] with
    def pack(e: Option[T], b: ByteBuffer): Unit =
      e match
        case Some(e) => b.put(sm.ordinal(e).toByte)
        case None => b.put(-1.toByte)

    def size: Int = 1

    def unpack(index: Int, b: IArray[Byte]): Option[T] =
      b(index).toInt match
        case -1 => None
        case v => Some(mirror.fromOrdinal(v).get)


  def pack[T: Pack](t: T): IArray[Byte] =
    val p = summon[Pack[T]]
    val buff = ByteBuffer.allocate(p.size)
    p.pack(t, buff)
    IArray.unsafeFromArray(buff.array())

  def unpack[T: Pack](b: IArray[Byte])(using m: Mirror.ProductOf[T]): T =
    val p = summon[Pack[T]]
    p.unpack(0, b)

  def indexOf[T: PackProduct](i: Int) = summon[PackProduct[T]].index(i)
  inline def indexOf[F: PackProduct](f: F => Any): Int = ${ FieldIndex.fieldIndexImpl[F]('{f}) }


  // TODO find a way to check field name at compile time and get field type
  def indexOf[T: PackProduct](field: String): Int =
    val pack = summon[PackProduct[T]]
    val index = pack.fields.getOrElse(field, throw RuntimeException(s"Field $field not found among ${pack.fields}"))
    indexOf[T](index)


  //inline def fieldName[F](inline f: F => Any) = FieldIndex.fieldName(f)

  def size[T: Pack] = summon[Pack[T]].size


  def packProduct[T](p: Mirror.ProductOf[T], elems: Array[Pack[_]], fieldsValue: Map[String, Int]): Pack[T] with PackProduct[T] =
    def packElement(elem: Pack[_])(x: Any, b: ByteBuffer): Unit =
      elem.asInstanceOf[Pack[Any]].pack(x, b)

    def unpackElement(elem: Pack[_])(index: Int, b: IArray[Byte]): Any =
      elem.asInstanceOf[Pack[Any]].unpack(index, b)

    val indexValue =
      val sizes = elems.map(_.size)
      Array.tabulate(sizes.length): x =>
        sizes.take(x).sum

    def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

    new Pack[T] with PackProduct[T]:
      def pack(e: T, b: ByteBuffer): Unit =
        iterator(e).zip(elems.iterator).foreach:
          case (e, elem) => packElement(elem)(e, b)

      lazy val size = elems.map(_.size).sum

      def unpack(index: Int, b: IArray[Byte]): T =
        def recurse(tuple: Tuple, index: Int, elemIndex: Int): Tuple =
          if elemIndex >= elems.length
          then tuple
          else
            val res = unpackElement(elems(elemIndex))(index, b)
            val h = elems(elemIndex)
            recurse(tuple :* res, index + h.size, elemIndex + 1)

        p.fromProduct(recurse(EmptyTuple, index, 0))

      def index: Array[Int] = indexValue
      lazy val fields = fieldsValue

  inline def summonAll[T <: Tuple]: List[Pack[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Pack[t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.ProductOf[T]): PackProduct[T] =
    lazy val fields = FieldIndex.fields[T]
    lazy val elemInstances = summonAll[m.MirroredElemTypes].toArray
    packProduct(m, elemInstances, fields)

trait Pack[T]:
  def pack(t: T, buffer: ByteBuffer): Unit
  def size: Int
  def unpack(index: Int, b: IArray[Byte]): T

trait PackProduct[T] extends Pack[T]:
  def index: Array[Int]
  lazy val fields: Map[String, Int]
