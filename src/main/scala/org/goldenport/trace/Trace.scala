package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable
import org.goldenport.exception.RAISE

/*
 * @since   Nov. 13, 2017
 * @version Nov. 13, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Trace {
  val timestamp = System.currentTimeMillis
  def asTree: Tree[Trace]
}

case class Root() extends Trace {
  override def toString() = s"[${timestamp}]/"
  def asTree: Tree[Trace] = RAISE.noReachDefect
}

case class Log(label: String) extends Trace {
  override def toString() = s"[${timestamp}]LOG:${label}"
  def asTree: Tree[Trace] = Tree.leaf(this)
}

class Invoke(label: String, enterMessage: String) extends Trace {
  private var _leave: Option[String] = None
  private var _leave_timestamp: Option[Long] = None
  private val _trace = mutable.ArrayBuffer.empty[Trace]

  override def toString() = s"[${timestamp}]INVOKE:${label} ${enterMessage} => ${leaveMessage}"

  def leave(p: Result[_]): Unit = {
    _leave = Some(p.leaveMessage)
    _leave_timestamp = Some(System.currentTimeMillis)
  }

  def trace(p: Trace): Unit = _trace += p

  def getLeaveMessage = _leave
  def leaveMessage = getLeaveMessage getOrElse "VOID"
  def asTree: Tree[Trace] = Tree.node(this, _trace.toStream.map(_.asTree))
}
