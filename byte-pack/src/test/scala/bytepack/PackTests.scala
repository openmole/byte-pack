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


import org.scalatest.funsuite.AnyFunSuite

import java.nio.ByteBuffer

object PackTests:
  enum En derives EnumMirror :
    case V1, V2

  case class TestClass(i: Int, x: Float, e: En, e2: Option[En], e3: Option[En])
  case class UpperClass(testClass: TestClass, j: Byte)

class PackTests extends AnyFunSuite:

  test("BytePack should work"):
    val buffer = ByteBuffer.allocate(4)
    buffer.putFloat(9.0)
    assert(BytePack.extractFloat(IArray.unsafeFromArray(buffer.array())) == 9.0f)

  test("Packing and unpacking should return the same case class"):
    import PackTests.*
    val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)
    val packed = Pack.pack(p)

    assert(p == Pack.unpack[UpperClass](packed))
    assert(Pack.unpack[UpperClass](_.j)(packed) == 8.toByte)

    def unpackMethod = Pack.unpack[UpperClass](_.j)
    assert(unpackMethod(packed) == 8.toByte)

    assert(Pack.unpack[UpperClass](_.testClass.i)(packed) == 9)
    assert(Pack.unpack[UpperClass](_.testClass.x)(packed) == 8.0)

  test("Index should be correct"):
    import PackTests.*
    val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)
    val packed = Pack.pack(p)
    assert(Pack.indexOf[UpperClass](1) == 11)
    assert(packed(Pack.indexOf[UpperClass](1)) == 8.toByte)
    assert(Pack.indexOf[UpperClass](_.j) == 11)

  test("Modify should work"):
    import PackTests.*
    val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)
    val packed = Pack.pack(p)

    val modifyX = Pack.modifier[UpperClass](_.testClass.x)
    val modifyJ = Pack.modifier[UpperClass](_.j)

    val newPacked = Pack.modify(packed, modifyX.set(20.0f), modifyJ.set(100.toByte))

    assert(Pack.unpack[UpperClass](packed) == p)
    assert(Pack.unpack[UpperClass](newPacked) == p.copy(testClass = p.testClass.copy(x = 20.0f), j = 100.toByte))