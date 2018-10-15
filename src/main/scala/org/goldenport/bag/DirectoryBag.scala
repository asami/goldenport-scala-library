package org.goldenport.bag

import java.io.File

/*
 * @since   Jul. 28, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
trait DirectoryBagBase extends Bag {
  def homeDirectory: File
  def createChunkBag: ChunkBag = throw new UnsupportedOperationException("zip")
  def getChunkBag: Option[ChunkBag] = None
}

class DirectoryBag(val homeDirectory: File) extends DirectoryBagBase {
}

object DirectoryBag {
  def createFromZip(home: File, zip: File): DirectoryBag = {
    ???
  }
}
