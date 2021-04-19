package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable
import org.goldenport.exception.RAISE
import org.goldenport.context._
import org.goldenport.extension.Showable
import org.goldenport.extension.IRecord

/*
 * @since   Nov. 13, 2017
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 * @version Apr.  6, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Trace extends Showable {
  def kind: String
  def print = toString
  def display = print
  def show = display
  def embed = show
  val timestamp = System.currentTimeMillis
  lazy val timestampkey = timestamp.toString.takeRight(5)
  def endTimestamp: Option[Long] = None
  def asTree: Tree[Trace] = RAISE.noReachDefect
  def asTreeSplint(start: Long): Tree[String] = RAISE.noReachDefect
  def incidents: Incidents = RAISE.notImplementedYetDefect

  def properties: IRecord = IRecord.data(
    "kind" -> kind
  ) + trace_Properties

  protected def trace_Properties: IRecord = IRecord.empty
}
object Trace {
  val empty = Empty

  class Printer(f: Trace => String = _.print, indentWidth: Int = 2) {
    private val _newline = "\n" // TODO
    private val _buffer = new StringBuilder()

    def print(p: Trace): String = {
      _buffer.append(f(p))
      _buffer.append(_newline)
      p match {
        case m: Container => m.children.foreach(_print(1, _))
        case m => // do nothing
      }
      _buffer.toString()
    }

    private def _print(depth: Int, p: Trace) {
      _buffer.append(Vector.fill(depth * indentWidth)(' ').mkString)
      _buffer.append(f(p))
      _buffer.append(_newline)
      p match {
        case m: Container => m.children.foreach(_print(depth + 1, _))
        case m => // do nothing
      }
    }
  }
  object Printer {
    def print(p: Trace): String = {
      val printer = new Printer(_.print)
      printer.print(p)
    }

    def display(p: Trace): String = {
      val printer = new Printer(_.display)
      printer.print(p)
    }

    def show(p: Trace): String = {
      val printer = new Printer(_.show)
      printer.print(p)
    }

    def embed(p: Trace): String = {
      val printer = new Printer(_.embed)
      printer.print(p)
    }
  }
}

sealed trait Container extends Trace {
  def isEmpty: Boolean = children.isEmpty
  def children: List[Trace]

  override protected final def trace_Properties: IRecord = {
    val t = children.nonEmpty option children.map(_.properties)
    IRecord.dataOption(
      "trace" -> t
    ) + trace_Container_Properties
  }

  protected def trace_Container_Properties: IRecord
}

case object Empty extends Trace {
  val kind = "empty"
  override def toString() = s"[${timestampkey}]/"
}

class Root() extends Container {
  val kind = "root"
  private var _children: List[Trace] = Nil
  def children = _children
  override def toString() = s"[${timestampkey}]/"
  override def display = "/"

  def set(ps: Seq[Trace]): Root = {
    _children = ps.toList
    this
  }

  protected def trace_Container_Properties: IRecord = IRecord.empty
}

case class Log(label: String) extends Trace {
  val kind = "log"
  override def toString() = s"[${timestampkey}]LOG:${label}"
  override def asTree: Tree[Trace] = Tree.leaf(this)
  override def asTreeSplint(start: Long): Tree[String] = {
    val splint = timestamp - start
    Tree.leaf(s"[${splint}]LOG:${label}")
  }
}

class Invoke(label: String, enterMessage: String) extends Container {
  val kind = "invoke"
  private var _leave: Option[String] = None
  private var _leave_timestamp: Option[Long] = None
  private val _trace = mutable.ArrayBuffer.empty[Trace]
  override def endTimestamp: Option[Long] = _leave_timestamp
  lazy val lap = _leave_timestamp.map(_ - timestamp)
  lazy val lapkey = lap.fold("")(x => s"($x)")

  def children = _trace.toList

  override def toString() = s"[${timestampkey}${lapkey}]INVOKE:${label} ${enterMessage} => ${leaveMessage}"
  override def display = s"INVOKE${lapkey}[$label] ${enterMessage} => ${leaveMessage}"

  def leave(p: Result[_]): Unit = {
    _leave = Some(p.leaveMessage)
    _leave_timestamp = Some(System.currentTimeMillis)
  }

  def leave(p: String): Unit = {
    _leave = Some(p)
    _leave_timestamp = Some(System.currentTimeMillis)
  }

  def effect(p: Effect): Unit = trace(EffectTrace(p))
  def effect(ps: Seq[Effect]): Unit = trace(ps.map(EffectTrace))

  def fault(p: Fault): Unit = trace(FaultTrace(p))
  def fault(ps: Seq[Fault]): Unit = trace(ps.map(FaultTrace))

  def trace(p: Trace): Unit = _trace += p
  def trace(ps: Seq[Trace]): Unit = _trace ++= ps

  def getLeaveMessage = _leave
  def leaveMessage = getLeaveMessage getOrElse "VOID"
  override def asTree: Tree[Trace] = Tree.node(this, _trace.toStream.map(_.asTree))
  override def asTreeSplint(start: Long): Tree[String] = {
    val splint = timestamp - start
    Tree.node(s"[${splint}${lapkey}]INVOKE:${label} ${enterMessage} => ${leaveMessage}", _trace.toStream.map(_.asTreeSplint(start)))
  }

  protected def trace_Container_Properties: IRecord = IRecord.empty
}

case class EffectTrace(effect: Effect) extends Trace {
  val kind = "effect"
}

class FutureTrace() extends Container {
  val kind = "future"
  private var _effect: Option[Effect.FutureEffect] = None
  private var _children: Seq[Trace] = Nil

  def effect = _effect.get
  def children = _children.toList

  def setEffect(p: Effect.FutureEffect): FutureTrace = {
    _effect = Some(p)
    this
  }

  def setChildren(p: Seq[Trace]): FutureTrace = {
    _children = p
    this
  }

  protected def trace_Container_Properties: IRecord = IRecord.empty
}
object FutureTrace {
  // def create(p: Effect.FutureEffect): FutureTrace = {
  //   val t = new FutureTrace()
  //   val e = Effect.FutureEffect.create(t)
  //   t.futureEffect = Some(e)
  //   t
  // }
}

case class FaultTrace(fault: Fault) extends Trace {
  val kind = "fault"
}
