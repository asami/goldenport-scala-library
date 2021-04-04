package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable
import org.goldenport.extension.Showable
import org.goldenport.context.Fault
import org.goldenport.parser.ParseFailure

/*
 * @since   Nov. 13, 2017
 *  version Feb. 25, 2021
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
class TraceContext() extends Showable {
  private val _root = new Root()
  private val _trace = mutable.ArrayBuffer.empty[Trace]
  private var _stack: List[Invoke] = Nil

  def print = {
    // val printer = new TraceContext.Printer()
    // printer.make(_root)
    // printer.print
    showTree
  }
  def display = print
  def show = print
  def embed = print

  def isEmpty: Boolean = _root.isEmpty && _trace.isEmpty && _stack == Nil

  def toHandle: TraceHandle = TraceHandle(this)

  def execute[T](label: String, enter: String)(body: => Result[T]): T = {
    val t = new Invoke(label, enter)
    trace(t)
    _stack = t :: _stack
    val r = body
    t.leave(r)
    _stack = _stack.tail
    r.r
  }

  def executeOption[T](label: String, enter: String)(body: => Option[Result[T]]): Option[T] = {
    val t = new Invoke(label, enter)
    trace(t)
    _stack = t :: _stack
    body.map { x =>
      t.leave(x)
      _stack = _stack.tail
      Some(x.r)
    }.getOrElse {
      _stack = _stack.tail
      _trace.dropRight(1)
      None
    }
  }

  def enter(label: String, input: String): Unit = {
    val t = new Invoke(label, input)
    trace(t)
    _stack = t :: _stack
  }

  def leave(label: String, output: String): Unit = {
    val t = _stack.head.asInstanceOf[Invoke]
    t.leave(output)
    _stack = _stack.tail
  }

  def log(p: String): Unit = trace(Log(p))

  def trace(p: Trace): Unit = _stack.headOption.
    map(_.trace(p)).
    getOrElse(_trace += p)

  def trace(ps: Seq[Trace]): Unit = _stack.headOption.
    map(_.trace(ps)).
    getOrElse(_trace ++= ps)

  def fault(p: Fault): Unit = trace(FaultTrace(p))

  def fault(ps: Seq[Fault]): Unit = trace(ps.map(FaultTrace))

//  def argumentFault(p: ParseFailure[_]): Unit = ???

  def asTree: Tree[Trace] = Tree.node(_root, _trace.toStream.map(_.asTree))
  def asTreeSplint: Tree[String] = {
    val start = _root.timestamp
    val end = _trace.lastOption.flatMap(_.endTimestamp)
    val a = end.fold(s"$start")(x => s"$start(${x - start})")
    Tree.node(a, _trace.toStream.map(_.asTreeSplint(start)))
  }

  def showTree: String = {
    import Show._
    implicit def showtrace = Show.showA[Trace]
    asTree.drawTree
  }

  def showTreeSplint: String = {
    // TODO remove double-quotes from showing string.
    asTreeSplint.drawTree
  }

  def toTrace: Trace = _root.set(_trace)
}

object TraceContext {
  def create(): TraceContext = new TraceContext()

  // class Printer(
  //   val indentwidth: Int = 2
  // ) {
  //   private var _depth = 0
  //   private val _buffer = new StringBuilder()

  //   def make(p: Trace) {
  //     _buffer(p.print)
  //     p.
  //   }

  //   def print: String = _buffer.toString()
  // }
}

case class Result[T](r: T, leaveMessage: String) {
}
object Result {
  def apply[T](r: T, leaveMessage: Option[String]): Result[T] =
    Result(r, leaveMessage getOrElse "VOID")
}
