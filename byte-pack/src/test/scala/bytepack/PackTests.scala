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

object PackTests:
  enum En derives EnumMirror :
    case V1, V2

  case class TestClass(i: Int, x: Float, e: En, e2: Option[En], e3: Option[En])
  case class UpperClass(testClass: TestClass, j: Byte)

class PackTests extends AnyFunSuite:

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
