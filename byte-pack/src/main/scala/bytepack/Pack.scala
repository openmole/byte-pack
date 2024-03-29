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


  given [T](using mirror: EnumMirror[T], sm: Mirror.SumOf[T]): Pack[T] =
    new Pack[T]:
      def pack(e: T, b: ByteBuffer): Unit = b.put(sm.ordinal(e).toByte)
      def size: Int = 1
      def unpack(index: Int, b: IArray[Byte]): T = mirror.fromOrdinal(b(index).toInt).get

  def pack[T: Pack](t: T): IArray[Byte] =
    val p = summon[Pack[T]]
    val buff = ByteBuffer.allocate(p.size)
    p.pack(t, buff)
    IArray.unsafeFromArray(buff.array())

  def unpack[T: Pack](b: IArray[Byte])(using m: Mirror.ProductOf[T]): T =
    val p = summon[Pack[T]]
    p.unpack(0, b)

  def indexOf[T: Pack](i: Int) = summon[Pack[T]].index(i)

  def size[T: Pack] = summon[Pack[T]].size

//  def indexOf[T: Pack](name: String)(using mirror: Mirror.ProductOf[T]): Unit =
//    println(constValueTuple[mirror.MirroredElemLabels].toList)
//    ???
//    //summon[Pack[T]].index(i)

  def packProduct[T](p: Mirror.ProductOf[T], elems: Array[Pack[_]]): Pack[T] =
    def packElement(elem: Pack[_])(x: Any, b: ByteBuffer): Unit =
      elem.asInstanceOf[Pack[Any]].pack(x, b)

    def unpackElement(elem: Pack[_])(index: Int, b: IArray[Byte]): Any =
      elem.asInstanceOf[Pack[Any]].unpack(index, b)

    val indexValue =
      val sizes = elems.map(_.size)
      Array.tabulate(sizes.length): x =>
        sizes.take(x).sum

    def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

    new Pack[T]:
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

      override def index: Array[Int] = indexValue

  inline def summonAll[T <: Tuple]: List[Pack[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Pack[t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.ProductOf[T]): Pack[T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes].toArray
    packProduct(m, elemInstances)

trait Pack[T]:
  def pack(t: T, buffer: ByteBuffer): Unit
  def size: Int
  def unpack(index: Int, b: IArray[Byte]): T
  def index: Array[Int] = Array(0)

