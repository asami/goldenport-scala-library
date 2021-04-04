package org.goldenport.bag

import java.io._

/*
 * @since   Apr.  4, 2021
 * @version Apr.  4, 2021
 * @author  ASAMI, Tomoharu
 */
class BlobBag(
  bag: ChunkBag
) extends ChunkBag {
  def openInputStream(): InputStream = bag.openInputStream()

  def openOutputStream(): OutputStream = bag.openOutputStream()

  override def size: Long = bag.size
  override def getSize = bag.getSize
  override def isEmpty = bag.isEmpty
}

object BlobBag {
  def create(p: ChunkBag): BlobBag = new BlobBag(p)
  def create(p: String): BlobBag = new BlobBag(StringBag.create(p))
}
