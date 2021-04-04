package org.goldenport.console

import scalaz._, Scalaz._
import org.goldenport.Strings

/*
 * @since   Jan. 10, 2021
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class MessageSequence(
  messages: Vector[Message]
) {
  def isEmpty: Boolean = messages.isEmpty
  def +(p: MessageSequence): MessageSequence = MessageSequence(messages ++ p.messages)
  def :+(p: Message): MessageSequence = copy(messages :+ p)
}

object MessageSequence {
  val empty = MessageSequence(Vector.empty)

  implicit object MessageSequenceMonoid extends Monoid[MessageSequence] {
    def zero = empty
    def append(lhs: MessageSequence, rhs: => MessageSequence) = lhs + rhs
  }

  def apply(p: Message): MessageSequence = MessageSequence(Vector(p))

  def apply(p: Message, p2: Message, ps: Message*): MessageSequence = 
    MessageSequence(Vector(p, p2) ++ ps)

  def apply(p: String): MessageSequence = MessageSequence(Vector(Message(p)))

  def apply(p: String, p2: String, ps: String*): MessageSequence =
    MessageSequence(Vector(Message(p), Message(p2)) ++ ps.map(Message.apply))

  def createNormalized(p: String): MessageSequence = MessageSequence(_normalize(p))

  def createNormalized(p: String, p2: String, ps: String*): MessageSequence =
    createNormalized(p +: ps ++: ps)

  def createNormalized(ps: Seq[String]): MessageSequence =
    MessageSequence(ps.flatMap(_normalize).toVector)

  private def _normalize(p: String): Vector[Message] = Strings.tolines(p).map(Message.apply)

  def createErrorNormalized(p: String): MessageSequence = MessageSequence(_normalize(p))

  def createErrorNormalized(p: String, p2: String, ps: String*): MessageSequence =
    createNormalized(p +: ps ++: ps)

  def createErrorNormalized(ps: Seq[String]): MessageSequence =
    MessageSequence(ps.flatMap(_normalize_error).toVector)

  private def _normalize_error(p: String): Vector[Message] = Strings.tolines(p).map(ErrorMessage.apply)

  def createWarningNormalized(p: String): MessageSequence = MessageSequence(_normalize(p))

  def createWarningNormalized(p: String, p2: String, ps: String*): MessageSequence =
    createNormalized(p +: ps ++: ps)

  def createWarningNormalized(ps: Seq[String]): MessageSequence =
    MessageSequence(ps.flatMap(_normalize_warning).toVector)

  private def _normalize_warning(p: String): Vector[Message] = Strings.tolines(p).map(WarningMessage.apply)
}

