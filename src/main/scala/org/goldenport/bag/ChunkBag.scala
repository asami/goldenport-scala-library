package org.goldenport.bag

import scala.util.Try
import scalaz.stream.{Writer => _, _}
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import java.io._
import java.nio.charset.Charset
import scalax.file.Path
import scalax.io._
import scalax.io.managed.InputStreamResource
import scodec.bits.ByteVector
import com.asamioffice.goldenport.io.UIO

/*
 * @since   Jun.  8, 2014
 *  version Oct. 27, 2014
 *  version Nov. 12, 2014
 *  version Dec. 30, 2014
 *  version Sep. 29, 2015
 *  version Oct.  5, 2015
 *  version Feb. 29, 2016
 *  version Sep. 22, 2016
 *  version Jul. 24, 2017
 *  version Aug. 30, 2017
 * @version Oct.  5, 2018
 * @author  ASAMI, Tomoharu
 */
trait ChunkBag extends Bag {
  def getChunkBag = Some(this)
  def createChunkBag = copyTo(new BufferFileBag())

  def openInputStream(): InputStream
  def openOutputStream(): OutputStream

  def openReader(): java.io.Reader = {
    val c = getCodec getOrElse {
      throw new IllegalStateException("Missing codec")
    }
    new InputStreamReader(openInputStream, c.charSet)
  }

  def openReader(enc: String): java.io.Reader = {
    assert (getCodec.isEmpty, "Encoding mismatch $enc against getCodec")
    new InputStreamReader(openInputStream, Codec(enc).charSet)
  }

  def openReader(charset: Charset): java.io.Reader = {
    assert (getCodec.isEmpty, "Encoding mismatch $enc against getCodec")
    new InputStreamReader(openInputStream, charset)
  }

  def openReader(codec: Codec): java.io.Reader = {
    assert (getCodec.isEmpty, "Encoding mismatch $enc against getCodec")
    new InputStreamReader(openInputStream, codec.charSet)
  }

  def openWriter(): java.io.Writer = {
    val codec = getCodec getOrElse Codec.UTF8 // compatibility
    openWriter(codec)
  }

  // XXX conflict getCodec
  // BufferBag#openWriter
  private[bag] def openWriter(enc: String): java.io.Writer = {
    openWriter(Codec(enc))
  }

  // XXX conflict getCodec
  private def openWriter(charset: Charset): java.io.Writer = {
    new OutputStreamWriter(openOutputStream, charset)
  }

  // XXX conflict getCodec
  // CsvBag#openWriter
  def openWriter(codec: Codec): java.io.Writer = {
    new OutputStreamWriter(openOutputStream, codec.charSet)
  }

  /**
   * Not closing the InputStream.
   */
  def write(in: => InputStream): Unit = {
    for (out <- resource.managed(openOutputStream()))
      UIO.stream2stream(in, out)
  }

  def writeClose(p: => InputStream): Unit = {
    for {
      in <- resource.managed(p)
      out <- resource.managed(openOutputStream())
    } {
      UIO.stream2stream(in, out)
    }
  }

  def writeClose[T](r: InputResource[T]): Unit = {
    r.copyDataTo(Resource.fromOutputStream(openOutputStream()))
  }

  def write(f: OutputStream => Unit) {
    import resource._
    for (out <- managed(openOutputStream())) {
      f(out)
    }
  }

  def write(lines: String) {
    write(lines, Codec.UTF8)
  }

  // XXX conflict getCodec
  def write(lines: String, charset: Charset) {
    write(new ByteArrayInputStream(lines.getBytes(charset)))
  }

  // XXX conflict getCodec
  def write(lines: String, codec: Codec) {
    write(new ByteArrayInputStream(lines.getBytes(codec.charSet)))
  }

  def write(bytes: Array[Byte]) {
    write(new ByteArrayInputStream(bytes))
  }

  def write(file: File) {
    write(new FileInputStream(file))
  }

  def chunkR: Channel[Task, Int, ByteVector] = io.chunkR(openInputStream())

  def chunksR: Process[Task, ByteVector] = chunksR(8192)

  def chunksR(buffersize: Int): Process[Task, ByteVector] = 
    Process.constant(buffersize).toSource.through(chunkR)

  def chunkW: Sink[Task, ByteVector] = io.chunkW(openOutputStream())

  def linesR: Process[Task, String] = linesR(Codec.UTF8)

  // XXX conflict getCodec
  private[bag] def linesR(enc: String): Process[Task, String] =
    linesR(Codec(enc))

  // XXX conflict getCodec
  private[bag] def linesR(charset: Charset): Process[Task, String] =
    io.linesR(openInputStream())(scala.io.Codec(charset))

  // XXX conflict getCodec
  // CsvBag
  def linesR(codec: Codec): Process[Task, String] =
    io.linesR(openInputStream())(scala.io.Codec(codec.charSet))

  def stringsR: Process[Task, String] = stringsR(Codec.UTF8)

  // XXX conflict getCodec
  private[bag] def stringsR(enc: String): Process[Task, String] =
    stringsR(Codec(enc))

  // XXX conflict getCodec
  private[bag] def stringsR(charset: Charset): Process[Task, String] =
    org.goldenport.stream.io.stringsR(openInputStream(), 8192)(scala.io.Codec(charset))

  // XXX conflict getCodec
  // CsvBag
  def stringsR(codec: Codec): Process[Task, String] =
    org.goldenport.stream.io.stringsR(openInputStream(), 8192)(scala.io.Codec(codec.charSet))

  def lineW: Sink[Task, String] = line_w(openWriter())

  // XXX conflict getCodec
  private[bag] def lineW(enc: String): Sink[Task, String] = line_w(openWriter(enc))

  // XXX conflict getCodec
  private[bag] def lineW(charset: Charset): Sink[Task, String] = line_w(openWriter(Codec(charset)))

  // XXX conflict getCodec
  private[bag] def lineW(codec: Codec): Sink[Task, String] = line_w(openWriter(codec))

  def copyTo[T <: ChunkBag](enc: String)(bag: T): T = {
    for (writer <- resource.managed(bag.openWriter(enc))) {
      copyTo(writer)
    }
    bag
  }

  def copyTo[T <: ChunkBag](bag: T): T = {
    for (out <- resource.managed(bag.openOutputStream)) {
      copyTo(out)
    }
    bag
  }

  def copyTo(sink: Sink[Task, ByteVector]): Unit = 
    chunksR.to(sink).run.run

  override def copyTo(out: OutputStream) {
    val bufsize = 8192
    val buf = new Array[Byte](bufsize)
    for (in <- resource.managed(openInputStream)) {
      var done = false
      do {
        val n = in.read(buf, 0, bufsize)
        if (n == 0 | n == -1)
          done = true
        else
          out.write(buf, 0, n)
      } while (!done)
    }
  }

  override def copyTo(out: Writer) {
    val bufsize = 8192
    val buf = new Array[Char](bufsize)
    for (in <- resource.managed(openReader)) {
      var done = false
      do {
        val n = in.read(buf, 0, bufsize)
        if (n == 0 | n == -1)
          done = true
        else
          out.write(buf, 0, n)
      } while (!done)
    }
  }

  def getFile: Option[File] = None
  def getSize: Option[Long] = None

  // XXX
  def toFile: File = getFile getOrElse {
    throw new UnsupportedOperationException("No file")
  }

  // XXX
  def size: Long = getSize getOrElse {
    throw new UnsupportedOperationException("Unknown size")
  }

  def isEmpty: Boolean = {
    getSize.map(_ == 0) getOrElse {
      throw new UnsupportedOperationException("Unknown size")
    }
  }

  def isEmpty(default: Boolean): Boolean = {
    getSize.map(_ == 0) getOrElse default
  }

  def nonEmpty = !isEmpty

  // naming convension for scala-arm
  def dispose(): Unit = Unit

  def toFileBag: FileBag = {
    val bag = new FileBag()
    copyTo(bag.chunkW)
    bag
  }

  def switchToFileBag: FileBag = {
    val f = toFileBag
    dispose()
    f
  }

  def toInputResource: InputResource[InputStream] = 
    new InputStreamResource(openInputStream(), closeAction = CloseAction((_: InputStream).close()))

  def toByteArray: Array[Byte] = {
    toInputResource.byteArray
  }

  def toText: String = {
    toInputResource.string(Codec.UTF8)
  }

  def toTextTry: Try[String] = {
    Try(toInputResource.string(Codec.UTF8))
  }

  def toTextTask: Task[String] = {
    Task(toInputResource.string(Codec.UTF8))
  }

  def toLines: scalax.io.LongTraversable[String] = {
    val codec = getCodec getOrElse Codec.UTF8
    toInputResource.lines()(codec)
  }

  protected def line_w(writer: => java.io.Writer): Sink[Task, String] = {
    def acquire = Task.delay(writer)
    def release(out: java.io.Writer) = Task.delay(out.close())
    def body(out: java.io.Writer) = Task.now { (line: String) =>
      Task.delay {
        out.write(line)
        out.write("\n")
      }
    }
    io.resource(acquire)(release)(body)
  }

  def createView(): ChunkBagView = {
    ChunkBagView(this, 0, None)
  }

  def createView(start: Long): ChunkBagView = {
    ChunkBagView(this, start, None)
  }

  def createView(start: Long, end: Long): ChunkBagView = {
    ChunkBagView(this, start, Some(end))
  }
}
