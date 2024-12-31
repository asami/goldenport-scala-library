package org.goldenport.bag

import java.io._
import org.goldenport.Platform
import org.goldenport.io.MimeType

/*
 * @since   Apr.  4, 2021
 *  version Feb.  1, 2022
 * @version Oct.  8, 2024
 * @author  ASAMI, Tomoharu
 */
class ClobBag(
  bag: ChunkBag,
  val designation: Bag.Designation = Bag.Designation.default
) extends ChunkBag with Bag.Designation.Impl {
  def update(p: Bag.Designation) = new ClobBag(bag, p)
  override def getCodec = designation.codec orElse bag.getCodec orElse Some(Platform.codec.UTF8)

  def openInputStream(): InputStream = bag.openInputStream()
  def openOutputStream(): OutputStream = bag.openOutputStream()

  override def size: Long = bag.size
  override def getSize = bag.getSize
  override def isEmpty = bag.isEmpty

  def isSame(p: ClobBag): Boolean =
    size == p.size && isSame(p.toText)

  def isSame(p: String): Boolean = bag.toText == p
}

object ClobBag {
  def create(p: ChunkBag): ClobBag = new ClobBag(p)
  def create(p: String): ClobBag = new ClobBag(StringBag.create(p))
  def create(name: String, suffix: String, mime: MimeType, p: ChunkBag): ClobBag =
    new ClobBag(p, Bag.Designation.create(name, suffix, mime))
}
