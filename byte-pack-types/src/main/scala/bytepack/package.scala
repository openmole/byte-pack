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


object UnsignedByte:
  val maxValue = Math.pow(2, 8)

  given Conversion[Byte, UnsignedByte] = identity
  given Conversion[Int, UnsignedByte] = apply

  extension (u: UnsignedByte)
    def value: Short = (u & 0xff).toShort
    inline def rawValue: Byte = u

  def apply(v: Int): UnsignedByte =
    assert(v < maxValue)
    v.asInstanceOf[Byte]

  inline def cast(byte: Byte): UnsignedByte = identity(byte)

opaque type UnsignedByte = Byte

object UnsignedShort:
  val maxValue = Math.pow(2, 16)

  given Conversion[Short, UnsignedShort] = identity
  given Conversion[Int, UnsignedShort] = apply

  def apply(v: Int): UnsignedShort =
    assert(v < maxValue)
    v.asInstanceOf[Short]

  inline def cast(s: Short): UnsignedShort = identity(s)

  extension (u: UnsignedShort)
    def value: Int = java.lang.Short.toUnsignedInt(u)
    inline def rawValue: Short = u

opaque type UnsignedShort = Short

object FixedSizeIArray:
  given [T, Size <: Int]: Conversion[IArray[T], FixedSizeIArray[T, Size]] = identity

  inline def apply[Size <: Int] =
    [T] => (a: IArray[T]) => (a: FixedSizeIArray[T, Size])

  extension [T, Size <: Int] (f: FixedSizeIArray[T, Size])
    def value: IArray[T] = f

opaque type FixedSizeIArray[T, Size <: Int] = IArray[T]

