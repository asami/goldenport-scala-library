package org.goldenport.bag

import scalaz.stream._
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import scalax.io._
import java.io._
import java.nio.ByteBuffer
import java.net.URL
import scodec.bits.ByteVector
import com.asamioffice.goldenport.io.UURL
// import org.goldenport.record.v2.InputFile
import org.goldenport.RAISE
import org.goldenport.io.MimeType
import org.goldenport.extension.IRecord
import org.goldenport.util.StringUtils

/*
 * @since   Jun.  8, 2014
 *  version Oct. 15, 2014
 *  version Nov. 12, 2014
 *  version Sep. 29, 2015
 *  version Sep. 22, 2016
 *  version Aug. 29, 2017
 *  version Sep. 17, 2018
 *  version Oct.  5, 2018
 *  version May. 21, 2020
 * @version Jun. 11, 2021
 * @author  ASAMI, Tomoharu
 */
class BufferFileBag(
  val bufferSize: Int = 65536,
  val nature: Nature = Nature.default
) extends ChunkBag with Nature.Holder {
  private var _underling: ChunkBag = BufferBag.create(getCodec)

  def underling = _underling

  override def withName(p: String): ChunkBag = _create(nature.withName(p))

  override def withSuffix(p: String): ChunkBag = _create(nature.withSuffix(p))

  override def withFilename(p: String): ChunkBag = {
    val (name, suffix) = StringUtils.pathnameBodySuffix(p)
    _create(nature.withNameSuffix(name, suffix))
  }

  override def withMimetype(p: MimeType): ChunkBag = _create(nature.withMimetype(p))

  override def withCodec(p: Codec): ChunkBag = {
    val r = new BufferFileBag(bufferSize, nature.withCodec(p))
    r._underling = _underling.withCodec(p)
    r
  }

  override def withProperties(p: IRecord): ChunkBag = _create(nature.withProperties(p))

  private def _create(p: Nature): ChunkBag = {
    val r = new BufferFileBag(bufferSize, p)
    r._underling = _underling
    r
  }

  override def isEmpty: Boolean = {
    _underling match {
      case x: BufferBag => x.isEmpty
      case x: FileBag => false
    }
  }

  private def _update[T](body: => T) = {
    val r = body
    _underling match {
      case x: BufferBag =>
        if (x.size >= bufferSize) {
          _underling = x.toFileBag
        }
      case _ => Unit
    }
    r
  }

  private def _use_file {
    _underling match {
      case x: BufferBag => _underling = x.toFileBag
      case _ => Unit
    }
  }

  override def openInputStream(): InputStream = {
    _underling.openInputStream()
  }

  // slow
  override def openOutputStream(): OutputStream = {
    val out = _underling.openOutputStream()
    _underling match {
      case _: BufferBag => new BufferFileOutputStream(out)
      case _ => out
    }
  }

  override def write(in: => InputStream): Unit = {
    _update {
      _underling.write(in)
    }
  }

  override def writeClose[T](r: InputResource[T]) {
    _update {
      _underling.writeClose(r)
    }
  }

  override def chunkR: Channel[Task, Int, ByteVector] = _underling.chunkR
  override def chunksR(buffersize: Int): Process[Task, ByteVector] = _underling.chunksR(buffersize)
  override def chunkW: Sink[Task, ByteVector] = io.chunkW(openOutputStream)
  override def linesR: Process[Task, String] = _underling.linesR
  override def linesR(codec: Codec): Process[Task, String] = _underling.linesR(codec)
  override def getFile: Option[File] = _underling.getFile
  override def toFile: File = _underling.toFile
  override def size: Long = _underling.size
  override def dispose(): Unit = _underling.dispose()
  override def switchToFileBag: FileBag = _underling.switchToFileBag

  class BufferFileOutputStream(o: OutputStream) extends OutputStream {
    var out: OutputStream = o
    private def _optimize_stream[T](body: => T) = {
      val r = body
      _underling match {
        case x: BufferBag =>
          if (x.size >= bufferSize) {
            _underling = FileBag.create(getCodec)
            out.flush()
            out.close()
            out = _underling.openOutputStream()
            out.write(x.toByteArray)
          }
        case _ => Unit
      }
      r
    }

    override def write(b: Array[Byte]) {
      _optimize_stream {
        out.write(b)
      }
    }

    override def write(b: Array[Byte], off: Int, len: Int) {
      _optimize_stream {
        out.write(b, off, len)
      }
    }

    override def write(b: Int) {
      _optimize_stream {
        out.write(b)
      }
    }

    override def flush() {
      out.flush()
    }

    override def close() {
      out.close()
    }
  }
}

object BufferFileBag {
  def create(codec: Codec): BufferFileBag =
    new BufferFileBag(nature = Nature(codec))

  def fromUri(uri: String): BufferFileBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    fromUrl(url)
  }

  def fromFile(file: File): BufferFileBag = {
    val bag = new BufferFileBag()
    for (in <- resource.managed(new FileInputStream(file)))
      bag.write(in)
    bag
  }

  def fromUrl(url: URL): BufferFileBag = {
    val bag = new BufferFileBag()
    for (in <- resource.managed(url.openStream))
      bag.write(in)
    bag
  }

  def fromInputStream(in: InputStream): BufferFileBag = {
    val bag = new BufferFileBag()
    bag.write(in)
    bag
  }

  def fromInputStreamNoClose(p: InputStream): BufferFileBag =
    fromInputStream(p)

  def fromInputStreamAndClose(p: InputStream): BufferFileBag = {
    val bag = new BufferFileBag()
    for (in <- resource.managed(p)) 
      bag.write(in)
    bag
  }

  def fromByteBuffer(p: ByteBuffer): BufferFileBag = {
    val bag = new BufferFileBag()
    for (out <- resource.managed(bag.openOutputStream()))
      while (p.hasRemaining())
        out.write(p.get())
    bag
  }

  // def fromInputFile(file: InputFile): BufferFileBag = {
  //   val bag = new BufferFileBag()
  //   file.getUrl match {
  //     case Some(url) => fromUrl(url)
  //     case None =>
  //       for {
  //         f <- resource.managed(file.createWorkFile)
  //         in <- resource.managed(f.openStream)
  //       } {
  //         bag.write(in)
  //       }
  //   }
  //   bag
  // }
}
