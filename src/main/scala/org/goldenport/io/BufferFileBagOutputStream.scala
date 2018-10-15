package org.goldenport.io

import java.io.{OutputStream, FileOutputStream}
import org.goldenport.bag.BufferFileBag

/*
 * @since   Jun. 17, 2014
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
class BufferFileBagOutputStream(
  autoDispose: Boolean = true,
  onClose: BufferFileBag => Unit = _ => Unit
) extends ChunkBagOutputStreamBase(
  new BufferFileBag(), autoDispose, onClose
) {
}
