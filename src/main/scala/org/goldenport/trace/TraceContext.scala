package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable

/*
 * @since   Nov. 13, 2017
 * @version Nov. 14, 2017
 * @author  ASAMI, Tomoharu
 */
class TraceContext() {
  private val _root = new Root()
  private val _trace = mutable.ArrayBuffer.empty[Trace]
  private var _stack: List[Invoke] = Nil

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

  def log(p: String): Unit = trace(Log(p))

  def trace(p: Trace): Unit = _stack.headOption.
    map(_.trace(p)).
    getOrElse(_trace += p)

  def asTree: Tree[Trace] = Tree.node(_root, _trace.toStream.map(_.asTree))
  def asTreeSplit: Tree[String] = {
    val start = _root.timestamp
    val end = _trace.lastOption.flatMap(_.endTimestamp)
    val a = end.fold(s"$start")(x => s"$start(${x - start})")
    Tree.node(a, _trace.toStream.map(_.asTreeSplit(start)))
  }

  def showTree: String = {
    import Show._
    implicit def showtrace = Show.showA[Trace]
    asTree.drawTree
  }

  def showTreeSplit: String = {
    // TODO remove double-quotes from showing string.
    asTreeSplit.drawTree
  }
}

object TraceContext {
}

case class Result[T](r: T, leaveMessage: String) {
}
object Result {
  def apply[T](r: T, leaveMessage: Option[String]): Result[T] =
    Result(r, leaveMessage getOrElse "VOID")
}
