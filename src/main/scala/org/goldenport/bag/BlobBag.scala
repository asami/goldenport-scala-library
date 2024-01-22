package org.goldenport.bag

import java.io._
import org.goldenport.io.MimeType

/*
 * @since   Apr.  4, 2021
 * @version Feb.  1, 2022
 * @author  ASAMI, Tomoharu
 */
class BlobBag(
  bag: ChunkBag,
  val designation: Bag.Designation = Bag.Designation.default
) extends ChunkBag with Bag.Designation.Impl {
  def update(p: Bag.Designation) = new BlobBag(bag, p)

  def openInputStream(): InputStream = bag.openInputStream()
  def openOutputStream(): OutputStream = bag.openOutputStream()

  override def size: Long = bag.size
  override def getSize = bag.getSize
  override def isEmpty = bag.isEmpty
}

object BlobBag {
  def create(p: ChunkBag): BlobBag = new BlobBag(p)
  def create(p: String): BlobBag = new BlobBag(StringBag.create(p))
  def create(name: String, suffix: String, mime: MimeType, p: ChunkBag): BlobBag =
    new BlobBag(p, Bag.Designation.create(name, suffix, mime))
}
