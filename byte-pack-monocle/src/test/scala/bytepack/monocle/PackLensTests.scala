package bytepack.monocle

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


import enumextensions.EnumMirror
import org.scalatest.funsuite.AnyFunSuite

import java.nio.ByteBuffer

import bytepack.*

object PackLensTests:
  enum En derives EnumMirror:
    case V1, V2

  case class TestClass(i: Int, x: Float, e: En, e2: Option[En], e3: Option[En])
  case class UpperClass(testClass: TestClass, j: Byte)

class PackLensTests extends AnyFunSuite:

  test("Iso should work"):
    import PackLensTests.*
    val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)

    val iso = Pack.iso[UpperClass]

    val packed: IArray[Byte] = iso.get(p)
    assert(p == iso.reverse.get(packed))


  test("Lens should work"):
    import PackLensTests.*
    val p = UpperClass(TestClass(9, 8.0, En.V2, None, Some(En.V1)), 8.toByte)

    val packed = bytepack.Pack.pack(p)
    val lens = Pack.lens[UpperClass](_.testClass.x)
    assert(lens.get(packed) == 8.0)

