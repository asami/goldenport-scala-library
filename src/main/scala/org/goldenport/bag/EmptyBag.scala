package org.goldenport.bag

import scalaz.stream._
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import scalax.file.Path
import scalax.io._
import java.io._
import scodec.bits.ByteVector

/*
 * @since   Jun. 13, 2014
 *  version Oct. 15, 2014
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object EmptyBag extends ChunkBag {
  def openInputStream(): java.io.InputStream = new InputStream {
    def read() = -1
  }
  def openOutputStream(): java.io.OutputStream = throw new UnsupportedOperationException("EmptyBag#openOutputStream")
  override def size: Long = 0
  override def getSize: Option[Long] = Some(0)
  override def isEmpty: Boolean = true
  override def isEmpty(default: Boolean): Boolean = false  
}
