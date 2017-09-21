package org.goldenport.io

import java.io.{OutputStream, FileOutputStream}
import org.goldenport.bag.FileBag

/*
 * @since   Jun. 17, 2014
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
class FileBagOutputStream(
  autoDispose: Boolean = true,
  onClose: FileBag => Unit = _ => Unit
) extends ChunkBagOutputStreamBase(
  new FileBag(), autoDispose, onClose
) {
}
