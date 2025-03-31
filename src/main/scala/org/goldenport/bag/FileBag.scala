package org.goldenport.bag

import scalaz.stream._
import scalaz.concurrent.Task
import scodec.bits.ByteVector
import scalax.file.defaultfs.DefaultPath
import scalax.io._
import scalax.io.nio.SeekableFileChannel
import scalax.file.Path
import java.io._
import java.net.URL
import org.goldenport.Platform
import com.asamioffice.goldenport.io.UURL

/*
 * @since   Jun.  5, 2014
 *  version Jul. 22, 2014
 *  version Oct. 21, 2014
 *  version Nov. 12, 2014
 *  version Dec. 30, 2014
 *  version Sep. 29, 2015
 *  version Oct. 27, 2015
 *  version Sep. 22, 2016
 *  version Aug. 29, 2017
 *  version Oct.  5, 2018
 * @version Mar. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class FileBag(
  val file: DefaultPath = FileBag._create_temp_path(),
  override val getCodec: Option[Codec] = None
) extends ChunkBag with FilenameMimeFeature {
  private def _file = file

  protected def uri_Name = file.path

  override def openInputStream(): BufferedInputStream = {
    val in = _file.inputStream.open().get
    new BufferedInputStream(in)
  }

  override def openOutputStream(): BufferedOutputStream = {
    val out = _file.outputStream().open().get
    new BufferedOutputStream(out)
  }

  override def openWriter(encoding: String): BufferedWriter = {
    val out = _file.outputStream().open().get
    new BufferedWriter(new OutputStreamWriter(out, encoding))
  }

  override def writeClose[T](r: InputResource[T]) {
    r.copyDataTo(_file)
  }

  override def chunkR: Channel[Task, Int, ByteVector] = io.fileChunkR(_file.path)
  override def chunksR(buffersize: Int): Process[Task, ByteVector] =
    Process.constant(buffersize).toSource.through(io.fileChunkR(_file.path))
  override def chunkW: Sink[Task, ByteVector] = io.fileChunkW(_file.path)
  override def linesR: Process[Task, String] = io.linesR(_file.path)
  override def linesR(codec: Codec): Process[Task, String] =
    io.linesR(_file.path)(scala.io.Codec(codec.charSet))
  override def linesR(encoding: String): Process[Task, String] =
    linesR(Codec(encoding))

  override def switchToFileBag = this

  override def toFile: File = _file.jfile

  def toFilename = toFile.getAbsolutePath

  def revision: Option[Long] = toFile.lastModified match {
    case 0 => None
    case x => Some(x)
  }

  override def getSize: Option[Long] = Some(size)
  override def size: Long = toFile.length

  // naming convension for scala-arm
  override def dispose() {
    // println("FileBag#dispose")
    _file.delete()
  }

  def openFileChannelReadOnly: SeekableFileChannel = {
    import scalax.io.StandardOpenOption.Read
    scalax.io.support.FileUtils.openChannel(toFile, Vector(Read))
  }
}

object FileBag {
  def create(filename: String): FileBag = {
    new FileBag(Path.fromString(filename))
  }

  def create(file: java.io.File): FileBag = new FileBag(Path(file))

  def create(codec: Option[Codec]): FileBag =
    new FileBag(_create_temp_path, codec)

  def create(codec: Codec): FileBag =
    new FileBag(_create_temp_path, Some(codec))

  def fromUri(uri: String): FileBag = {
    val url = UURL.getURLFromFileOrURLName(uri)
    val bag = new FileBag()
    for {
      in <- resource.managed(url.openStream)
    } {
      bag.write(in)
    }
    bag
  }

  private[bag] def _create_temp_path() = Platform.createTempPath("filebag")
}
