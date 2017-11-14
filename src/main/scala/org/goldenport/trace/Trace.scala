package org.goldenport.trace

import scalaz._, Scalaz._
import scala.collection.mutable
import org.goldenport.exception.RAISE

/*
 * @since   Nov. 13, 2017
 * @version Nov. 14, 2017
 * @author  ASAMI, Tomoharu
 */
sealed trait Trace {
  val timestamp = System.currentTimeMillis
  lazy val timestampkey = timestamp.toString.takeRight(5)
  def endTimestamp: Option[Long] = None
  def asTree: Tree[Trace]
  def asTreeSplit(start: Long): Tree[String]
}

case class Root() extends Trace {
  override def toString() = s"[${timestampkey}]/"
  def asTree: Tree[Trace] = RAISE.noReachDefect
  def asTreeSplit(start: Long): Tree[String] = RAISE.noReachDefect
}

case class Log(label: String) extends Trace {
  override def toString() = s"[${timestampkey}]LOG:${label}"
  def asTree: Tree[Trace] = Tree.leaf(this)
  def asTreeSplit(start: Long): Tree[String] = {
    val splint = timestamp - start
    Tree.leaf(s"[${splint}]LOG:${label}")
  }
}

class Invoke(label: String, enterMessage: String) extends Trace {
  private var _leave: Option[String] = None
  private var _leave_timestamp: Option[Long] = None
  private val _trace = mutable.ArrayBuffer.empty[Trace]
  override def endTimestamp: Option[Long] = _leave_timestamp
  lazy val lap = _leave_timestamp.map(_ - timestamp)
  lazy val lapkey = lap.fold("")(x => s"($x)")

  override def toString() = s"[${timestampkey}${lapkey}]INVOKE:${label} ${enterMessage} => ${leaveMessage}"

  def leave(p: Result[_]): Unit = {
    _leave = Some(p.leaveMessage)
    _leave_timestamp = Some(System.currentTimeMillis)
  }

  def trace(p: Trace): Unit = _trace += p

  def getLeaveMessage = _leave
  def leaveMessage = getLeaveMessage getOrElse "VOID"
  def asTree: Tree[Trace] = Tree.node(this, _trace.toStream.map(_.asTree))
  def asTreeSplit(start: Long): Tree[String] = {
    val splint = timestamp - start
    Tree.node(s"[${splint}${lapkey}]INVOKE:${label} ${enterMessage} => ${leaveMessage}", _trace.toStream.map(_.asTreeSplit(start)))
  }
}
