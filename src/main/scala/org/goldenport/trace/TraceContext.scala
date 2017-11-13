package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable

/*
 * @since   Nov. 13, 2017
 * @version Nov. 13, 2017
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

  def log(p: String): Unit = trace(Log(p))

  def trace(p: Trace): Unit = _stack.headOption.
    map(_.trace(p)).
    getOrElse(_trace += p)

  def asTree: Tree[Trace] = Tree.node(_root, _trace.toStream.map(_.asTree))

  def showTree: String = {
    import Show._
    implicit def showtrace = Show.showA[Trace]
    asTree.drawTree
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
