package org.goldenport.io

import java.io.OutputStream
import org.goldenport.bag.ChunkBag

/*
 * @since   Jun. 17, 2014
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
abstract class ChunkBagOutputStreamBase[T <: ChunkBag](
  val bag: T,
  val autoDispose: Boolean,
  val onClose: T => Unit
) extends OutputStream {
  private val _out = bag.openOutputStream()

  override def write(b: Array[Byte]) {
    _out.write(b)
  }

  override def write(b: Array[Byte], off: Int, len: Int) {
    _out.write(b, off, len)
  }

  override def write(b: Int) {
    _out.write(b)
  }

  override def flush() {
    _out.flush()
  }

  override def close() {
    _out.close()
    onClose(bag)
    bag.dispose()
  }
}
