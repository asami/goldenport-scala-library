package org.goldenport.io

import java.io.{OutputStream}
import org.goldenport.bag.BufferBag

/*
 * @since   Jun. 17, 2014
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
class BufferBagOutputStream(
  autoDispose: Boolean = true,
  onClose: BufferBag => Unit = _ => Unit
) extends ChunkBagOutputStreamBase(
  new BufferBag(), autoDispose, onClose
) {
}
