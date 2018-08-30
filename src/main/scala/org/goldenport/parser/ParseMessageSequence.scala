package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 * @version Aug. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class ParseMessageSequence(
  messages: Vector[ParseMessage]
) {
  def +(rhs: ParseMessageSequence) = ParseMessageSequence(messages ++ rhs.messages)
}

object ParseMessageSequence {
  val empty = ParseMessageSequence(Vector.empty)

  implicit object ParseMessageSequenceMonoid extends Monoid[ParseMessageSequence] {
    def zero = ParseMessageSequence.empty
    def append(lhs: ParseMessageSequence, rhs: => ParseMessageSequence) = lhs + rhs
  }
}
