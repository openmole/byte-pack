package bp

import java.nio.ByteBuffer

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

object BytePack:
//  inline def fillBytes(inline size: Int, inline data: IArray[Byte]): IArray[Byte] =
//    val fill = size - data.length
//    IArray.tabulate(size): i =>
//      if i < fill
//      then 0
//      else data(i - fill)
//
//  inline def pack(inline byte: Byte): IArray[Byte] =
//    val buff = ByteBuffer.allocate(1)
//    buff.put(byte)
//    IArray.unsafeFromArray(buff.array())
//
//  inline def pack(inline short: Short): IArray[Byte] =
//    val buff = ByteBuffer.allocate(2)
//    buff.putShort(short)
//    fillBytes(2, IArray.unsafeFromArray(buff.array()))
//
//  inline def pack(inline int: Int): IArray[Byte] =
//    val buff = ByteBuffer.allocate(4)
//    buff.putInt(int)
//    fillBytes(4, IArray.unsafeFromArray(buff.array()))
//
//  inline def pack(inline float: Float): IArray[Byte] =
//    val buff = ByteBuffer.allocate(4)
//    buff.putFloat(float)
//    fillBytes(4, IArray.unsafeFromArray(buff.array()))

  inline def extractByte(inline b: IArray[Byte], inline index: Int = 0) = b(index)
  inline def extractShort(inline b: IArray[Byte], inline index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1))).getShort
  inline def extractInt(inline b: IArray[Byte], inline index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3))).getInt
  inline def extractFloat(inline b: IArray[Byte], inline index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3))).getFloat
  inline def extractDouble(inline b: IArray[Byte], inline index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3), b(index + 4), b(index + 5), b(index + 6), b(index + 7))).getDouble
  inline def extractLong(inline b: IArray[Byte], inline index: Int = 0) = ByteBuffer.wrap(Array[Byte](b(index), b(index + 1), b(index + 2), b(index + 3), b(index + 4), b(index + 5), b(index + 6), b(index + 7))).getLong

