package org.goldenport.bag

import java.io._
import org.goldenport.Platform

/*
 * @since   Apr.  4, 2021
 * @version Apr.  4, 2021
 * @author  ASAMI, Tomoharu
 */
class ClobBag(
  bag: ChunkBag
) extends ChunkBag {
  override def getCodec = bag.getCodec orElse Some(Platform.codec.UTF8)
  def openInputStream(): InputStream = bag.openInputStream()
  def openOutputStream(): OutputStream = bag.openOutputStream()

  override def size: Long = bag.size
  override def getSize = bag.getSize
  override def isEmpty = bag.isEmpty
}

object ClobBag {
  def create(p: ChunkBag): ClobBag = new ClobBag(p)
  def create(p: String): ClobBag = new ClobBag(StringBag.create(p))
}
