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

import bytepack.PackMacro.*

import scala.deriving.*
import scala.compiletime.*
import java.nio.ByteBuffer
import reflect.Selectable.reflectiveSelectable
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

export enumextensions.EnumMirror

object Pack:
  given Pack[Byte] with
    def pack(i: Byte, b: java.nio.ByteBuffer) = b.put(i)
    def size = 1
    def unpack(index: Int, b: IArray[Byte]) = BytePack.extractByte(b, index)

  given Pack[Short] with
    def pack(i: Short, b: java.nio.ByteBuffer) = b.putShort(i)
    def size = 2
    inline def unpack(index: Int, b: IArray[Byte]) = BytePack.extractShort(b, index)

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

  given Pack[UnsignedByte] with
    def pack(i: UnsignedByte, b: java.nio.ByteBuffer) = b.put(i.rawValue)
    def size = 1
    def unpack(index: Int, b: IArray[Byte]) = UnsignedByte.cast(bytepack.BytePack.extractByte(b, index))

  given Pack[UnsignedShort] with
    def pack(i: UnsignedShort, b: java.nio.ByteBuffer) = b.putShort(i.rawValue)
    def size = 2
    def unpack(index: Int, b: IArray[Byte]) = UnsignedShort.cast(bytepack.BytePack.extractShort(b, index))


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

  def immutableArray[T: ClassTag](s: Int)(using packT: Pack[T]): Pack[IArray[T]] = new Pack[IArray[T]]:
    override def size: Int = s * packT.size

    override def unpack(index: Int, b: IArray[Byte]): IArray[T] =
      val result = Array.ofDim[T](s)
      var i = 0
      while i < s
      do
        val eIndex = index + i * packT.size
        result(i) = packT.unpack(eIndex, b)
        i = i + 1

      IArray.unsafeFromArray(result)

    override def pack(a: IArray[T], b: ByteBuffer): Unit = a.foreach(e => packT.pack(e, b))

  given [T: ClassTag, S <: Int](using packA: Pack[T], s: ValueOf[S]): Pack[FixedSizeIArray[T, S]] = new Pack[FixedSizeIArray[T, S]]:
    inline def arrayPack = immutableArray(s.value)
    override def size: Int = arrayPack.size
    override def unpack(index: Int, b: IArray[Byte]): FixedSizeIArray[T, S] = FixedSizeIArray[S](arrayPack.unpack(index, b))
    override def pack(a: FixedSizeIArray[T, S], b: ByteBuffer): Unit = arrayPack.pack(a.value, b)
  
  def pack[T](t: T)(using packT: Pack[T]): IArray[Byte] =
    val buff = java.nio.ByteBuffer.allocate(packT.size)
    packT.pack(t, buff)
    IArray.unsafeFromArray(buff.array())

  def unpack[T: Pack](b: IArray[Byte]): T =
    val p = summon[Pack[T]]
    p.unpack(0, b)

  inline def indexOf[T: PackProduct](inline i: Int) = summon[PackProduct[T]].index(i)
  def indexOf[From]: MkFieldIndex[From] = new MkFieldIndex[From]() //${ FieldIndex.fieldIndexImpl[F]('{ f }) }

  def size[T: Pack] = summon[Pack[T]].size

  object Mutation:
    inline given Conversion[Array[Byte] => Unit, Mutation] = identity

  opaque type Mutation = Array[Byte] => Unit

  extension (m: Mutation)
    def apply(p: IArray[Byte]) = modify(p, m)

  trait Accessor[T]:
    def set(v: T): Mutation
    def modify(f: T => T): Mutation
    def get: Get[T]

  def access[From]: MkAccessorField[From] = new MkAccessorField[From]

  def modify(p: IArray[Byte], mutation: Mutation*): IArray[Byte] =
    val arr = p.toArray
    mutation.foreach: m =>
      m(arr)
    IArray.unsafeFromArray(arr)

  object Get:
    inline given [T]: Conversion[IArray[Byte] => T, Get[T]] = identity

  opaque type Get[T] = IArray[Byte] => T

  extension [T](a: Get[T])
    def apply(h: IArray[Byte]) = a(h)

  def packProduct[T](p: Mirror.ProductOf[T], elems: => Array[Pack[_]]): Pack[T] with PackProduct[T] =
    inline def packElement(elem: Pack[_])(x: Any, b: ByteBuffer): Unit =
      elem.asInstanceOf[Pack[Any]].pack(x, b)

    inline def unpackElement(elem: Pack[_])(index: Int, b: IArray[Byte]): Any =
      elem.asInstanceOf[Pack[Any]].unpack(index, b)

    inline def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

    lazy val indexValue =
      val sizes = elems.map(_.size)
      IArray.tabulate(sizes.length): x =>
        sizes.take(x).sum

    new Pack[T] with PackProduct[T]:
      def pack(e: T, b: ByteBuffer): Unit =
        iterator(e).zip(elems.iterator).foreach:
          case (e, elem) => packElement(elem)(e, b)

      lazy val size = elems.map(_.size).sum

      def unpack(index: Int, b: IArray[Byte]): T =
        val length = elems.length
        @tailrec def recurse(tuple: Tuple, index: Int, elemIndex: Int): Tuple =
          if elemIndex >= length
          then tuple
          else
            val h = elems(elemIndex)
            val res = unpackElement(h)(index, b)
            recurse(tuple :* res, index + h.size, elemIndex + 1)

        p.fromProduct(recurse(EmptyTuple, index, 0))

      inline def index(i: Int): Int = indexValue(i)

  inline def summonAll[T <: Tuple](buffer: mutable.ArrayBuffer[Pack[_]]): Array[Pack[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => buffer.toArray
      case _: (t *: ts) =>
        buffer.addOne(summonInline[Pack[t]])
        summonAll[ts](buffer)

  inline given derived[T](using m: Mirror.ProductOf[T]): PackProduct[T] =
    lazy val elems = summonAll[m.MirroredElemTypes](new mutable.ArrayBuffer[Pack[_]](100))
    packProduct(m, elems)

trait Pack[T]:
  def pack(t: T, buffer: ByteBuffer): Unit
  def size: Int
  def unpack(index: Int, b: IArray[Byte]): T

trait PackProduct[T] extends Pack[T]:
  def index(i: Int): Int
