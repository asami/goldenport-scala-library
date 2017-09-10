package org.goldenport.bag

import scalaz.stream._
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.Charset
import scalax.file.Path
import scalax.io._
import java.io._
import scodec.bits.ByteVector
import com.asamioffice.goldenport.io.UURL

/*
 * @since   Jun.  8, 2014
 *  version Oct. 15, 2014
 *  version Nov. 12, 2014
 *  version Sep. 22, 2016
 *  version Jul. 24, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
class BufferBag(
  initsize: Int = 8192,
  override val getCodec: Option[Codec] = None
) extends ChunkBag {
  private val _buffer = new ByteArrayOutputStream(initsize)

  def openInputStream(): InputStream = {
    new ByteArrayInputStream(_buffer.toByteArray)
  }

  def openOutputStream(): OutputStream = _buffer

  override def openWriter(encoding: String): java.io.Writer = {
    new OutputStreamWriter(_buffer, encoding)
  }

  override def openWriter(codec: Codec): java.io.Writer = {
    new OutputStreamWriter(_buffer, codec.charSet)
  }

  override def size: Long = _buffer.size
  override def getSize = Some(size)
  override def isEmpty = size == 0

  override def toByteArray: Array[Byte] = _buffer.toByteArray
}

object BufferBag {
  def create(codec: Option[Codec]): BufferBag = new BufferBag(8192, codec)

  def create(text: String, enc: String): BufferBag = {
    val codec = Charset.forName(enc)
    val bag = new BufferBag(8192, Some(codec))
    bag.write(text)
    bag
  }

  def fromUri(uri: String): BufferBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    val bag = new BufferBag()
    for {
      in <- resource.managed(url.openStream)
    } {
      bag.write(in)
    }
    bag
  }
}
