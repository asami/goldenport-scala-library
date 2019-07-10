package org.goldenport.bag

import scalaz.stream._
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import scalax.io.Codec
import java.io._
import org.goldenport.RAISE
import org.goldenport.Platform
import org.goldenport.io.IoUtils

/*
 * @since   Jun. 24, 2019
 * @version Jun. 24, 2019
 * @author  ASAMI, Tomoharu
 */
class StringBag(
  string: String,
  override val getCodec: Option[Codec] = None
) extends ChunkBag {
  private def _charset = getCodec.map(_.charSet).getOrElse(Platform.charset.UTF8)

  def openInputStream(): InputStream = IoUtils.toInputStream(string, _charset)

  def openOutputStream(): OutputStream = RAISE.unsupportedOperationFault

  override def size: Long = string.size
  override def getSize = Some(size)
  override def isEmpty = size == 0

  override lazy val toByteArray: Array[Byte] = string.getBytes(_charset)
}

object StringBag {
  def create(p: String): StringBag = new StringBag(p)
}
