package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 *  version Oct. 14, 2018
 *  version Nov. 18, 2018
 *  version Jul.  7, 2019
 * @version Jan. 16, 2021
 * @author  ASAMI, Tomoharu
 */
case class ParseMessageSequence(
  messages: Vector[ParseMessage]
) {
  def +(rhs: ParseMessageSequence) = ParseMessageSequence(messages ++ rhs.messages)
  def :+(p: ParseMessage) = ParseMessageSequence(messages :+ p)
  def +:(p: ParseMessage) = ParseMessageSequence(p +: messages)
  def :++(ps: Seq[ParseMessage]) = ParseMessageSequence(messages ++ ps)
  def ++:(ps: Seq[ParseMessage]) = ParseMessageSequence(ps ++: messages)

  def errors: Vector[ErrorMessage] = messages.collect {
    case m: ErrorMessage => m
  }

  def warnings: Vector[WarningMessage] = messages.collect {
    case m: WarningMessage => m
  }
}

object ParseMessageSequence {
  val empty = ParseMessageSequence(Vector.empty)

  implicit object ParseMessageSequenceMonoid extends Monoid[ParseMessageSequence] {
    def zero = ParseMessageSequence.empty
    def append(lhs: ParseMessageSequence, rhs: => ParseMessageSequence) = lhs + rhs
  }

  def error(location: Option[ParseLocation], p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(ErrorMessage(location, p)))

  def error(location: ParseLocation, p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(ErrorMessage(location, p)))

  def error(p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(ErrorMessage(p)))

  def error(p: String, p2: String, ps: String*): ParseMessageSequence =
    ParseMessageSequence((p +: p2 +: ps.toVector).map(ErrorMessage.apply))

  def warning(location: Option[ParseLocation], p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(WarningMessage(location, p)))

  def warning(location: ParseLocation, p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(WarningMessage(location, p)))

  def warning(p: String): ParseMessageSequence =
    ParseMessageSequence(Vector(WarningMessage(p)))

  def warning(p: String, p2: String, ps: String*): ParseMessageSequence =
    ParseMessageSequence((p +: p2 +: ps.toVector).map(WarningMessage.apply))
}
