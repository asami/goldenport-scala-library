package org.goldenport.io

import scalaz._, Scalaz._
import scalaz.stream._
import scalaz.concurrent.Task
import scala.collection.mutable.ArrayBuffer
import scalax.io.SeekableByteChannel
import java.io.InputStream
import java.nio.ByteBuffer
import scodec.bits.ByteVector
import org.goldenport.bag.{
  ChunkBag, ChunkBagView, BufferBag, BufferFileBag, FileBag
}

/*
 * @since   Oct.  5, 2015
 * @version Sep. 21, 2017
 * @author  ASAMI, Tomoharu
 */
class ChunkBagSplitter[T](bag: ChunkBag, threasholdSize: Int) {
  import ChunkBagSplitter._

  def views(): Seq[ChunkBagView] = {
    splitViews(bag, threasholdSize)
  }

  def split(): Stream[ChunkBag] = {
    resource.managed(_open_input_stream()) acquireAndGet { is =>
      val a = splitInputStreamWithoutCloseWithoutGather(is)
      _go(a)
    }
  }

  private def _open_input_stream(): InputStream = {
    bag.openInputStream()
  }

  private def _go(a: Seq[Task[ChunkBag]]): Stream[ChunkBag] = {
    a.headOption.cata(_.run #:: _go(a.tail), Stream.empty)
  }
}

object ChunkBagSplitter {
  val DEFAULT_THREASHOLD_SIZE = 50 * 1024 * 1024 // 50M byte

  sealed trait Event
  case object EndEvent extends Event
  case class InputByteArray(bytes: Array[Byte]) extends Event
  case class InputByteVector(bytes: ByteVector) extends Event

  sealed trait ProcessState {
    def apply(event: Event): (ProcessState, Seq[Task[ChunkBag]])
    protected def create_bag(): ChunkBag = new BufferFileBag()
  }

  case object InitState extends ProcessState {
    def apply(event: Event) = {
      event match {
        case EndEvent => (InitState, Nil)
        case evt: InputByteArray => NeutralState(create_bag(), Vector.empty).apply(evt)
        case evt: InputByteVector => NeutralState(create_bag(), Vector.empty).apply(evt)
      }
    }
  }

  case class NeutralState(bag: ChunkBag, bags: Vector[Task[ChunkBag]]) extends ProcessState {
    def apply(event: Event) = {
      event match {
        case EndEvent => (InitState, bags :+ Task.now(bag))
        case InputByteArray(bytes) => ???
        case InputByteVector(bytes) => ???
      }
    }
  }

  def splitInputStreamWithCloseWithoutGather(in: InputStream): IndexedSeq[Task[ChunkBag]] = {
    val a = io.chunkR(in)
    val b = Process.constant(8192).toSource.through(a)
    val c = b.pipe(fsm(InitState))
    val d: Task[IndexedSeq[Task[ChunkBag]]] = c.runLog.map(_.flatten)
    val e: IndexedSeq[Task[ChunkBag]] = d.run
    e
  }

  def splitInputStreamWithoutCloseWithoutGather(in: InputStream): Seq[Task[ChunkBag]] = {
    val a: Stream[Array[Byte]] = Stream.continually {
      val buf = new Array[Byte](8192)
      val size = in.read(buf)
      if (size > 0)
        buf
      else
        null
    }.takeWhile(_ != null)
    val b: (ProcessState, Stream[Seq[Task[ChunkBag]]]) = a.traverseS(action).run(InitState)
    val c: (ProcessState, Seq[Task[ChunkBag]]) = b._1.apply(EndEvent)
    c._2
  }

  def splitInputStreamWithCloseWithGather(in: InputStream): Task[List[ChunkBag]] = {
    val a = io.chunkR(in)
    val b = Process.constant(8192).toSource.through(a)
    val c = b.pipe(fsm(InitState))
    val d: Task[IndexedSeq[Seq[Task[ChunkBag]]]] = c.runLog
    val e: Task[Task[List[ChunkBag]]] = d.map { x =>
      Nondeterminism[Task].gather(x.flatten)
    }
    val f: Task[List[ChunkBag]] = e.join
    f
  }

  def fsm(state: ProcessState): Process1[ByteVector, Seq[Task[ChunkBag]]] = {
    Process.receive1 { in: ByteVector =>
      val (s, out) = state.apply(InputByteVector(in))
      Process.emit(out) fby fsm(s)
    }
  }

  def action(event: Array[Byte]): State[ProcessState, Seq[Task[ChunkBag]]] = State((s: ProcessState) => s.apply(InputByteArray(event)))

  // View
  def splitViews(bag: ChunkBag, threasholdSize: Int): Seq[ChunkBagView] = {
    bag match {
      case m: FileBag => splitViews(m, threasholdSize)
      case m: BufferFileBag => splitViews(m.underling, threasholdSize)
      case m: BufferBag => Vector(m.createView())
    }
  }

  def splitViews(bag: FileBag, threasholdSize: Int): Seq[ChunkBagView] = {
    splitViews(bag.openFileChannelReadOnly, threasholdSize).map {
      case (s, e) => bag.createView(s, e)
    }
  }

  def splitViews(channel: SeekableByteChannel, threasholdsize: Int): Seq[(Long, Long)] = {
    _split_views(channel, threasholdsize, 0, Vector.empty)
  }

  @annotation.tailrec
  def _split_views(
    channel: SeekableByteChannel,
    threasholdsize: Int,
    index: Long,
    views: Vector[(Long, Long)]
  ): Vector[(Long, Long)] = {
    val candidate = index + threasholdsize
    val size = channel.size
    if (size == index) {
      views
    } else if (size <= candidate) {
      views :+ (index, size)
    } else {
      channel.position(candidate)
      val buf = ByteBuffer.allocate(8192)
      val size = channel.read(buf)
      if (size > 0) {
        val exceeds = _calc(buf, size)
        val lastexclude = candidate + exceeds
        _split_views(channel, threasholdsize, lastexclude, views :+ (index, lastexclude))
      } else {
        views
      }
    }
  }

  private def _calc(buf: ByteBuffer, size: Int): Int = {
    _calc(buf.array, 0, size)
  }

  @annotation.tailrec
  private def _calc(buf: Array[Byte], index: Int, size: Int): Int = {
    if (index == size) {
      size
    } else {
      buf(index) match {
        case '\r' => _calc(buf, index + 1, size)
        case '\n' => _calc(buf, index + 1, size)
        case x => index
      }
    }
  }
}
