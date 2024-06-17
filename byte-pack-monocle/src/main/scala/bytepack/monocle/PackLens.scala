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

import monocle.*
import bytepack.*

object PackLens:
  class MkLens[From]:
    transparent inline def apply[To](inline f: From => To) =
      val access = Pack.access[From](f)
      Lens[IArray[Byte], To](access.get.apply)(t => b => access.set(t)(b))

  inline def apply[From] = new MkLens[From]

object PackIso:
  inline def apply[T](using p: Pack[T]) = Iso[T, IArray[Byte]](Pack.pack[T])(Pack.unpack[T])

extension (p: Pack.type)
  transparent inline def lens[From] = PackLens.apply[From]
  transparent inline def iso[T: Pack] = PackIso.apply[T]