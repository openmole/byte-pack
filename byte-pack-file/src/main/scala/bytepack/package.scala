package bytepack

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.reflect.ClassTag
import scala.util.*

/*
 * Copyright (C) 2026 Romain Reuillon
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

def save[T: Pack as pack](a: IArray[T], file: java.io.File) =
  file.delete()
  file.getParentFile.mkdirs()

  Using(GZIPOutputStream(BufferedOutputStream(FileOutputStream(file)))): os =>
    for
      e <- a
    do os.write(Pack.pack(e).unsafeArray)
  .get

def load[T: {Pack as pack, ClassTag}](file: java.io.File): IArray[T] =
  Using(GZIPInputStream(BufferedInputStream(FileInputStream(file)))): is =>
    val size = pack.size

    def readChunk(is: java.io.InputStream, size: Int): Array[Byte] = boundary:
      val buf = new Array[Byte](size)
      var off = 0
      while off < size
      do
        val n = is.read(buf, off, size - off)
        if n < 0 then boundary.break(java.util.Arrays.copyOf(buf, off))
        off += n
      buf

    IArray.unsafeFromArray:
      Iterator
        .continually(readChunk(is, size))
        .takeWhile(_.nonEmpty)
        .map(a => pack.unpack(0, IArray.unsafeFromArray(a)))
        .toArray
  .get

