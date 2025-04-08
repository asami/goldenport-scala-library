package org.goldenport.util

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/*
 * @since   Apr.  5, 2025
 * @version Apr.  5, 2025
 * @author  ASAMI, Tomoharu
 */
case class RegexMatchSequence(
  vector: Vector[RegexMatchSequence.Slot] = Vector.empty
) {
  def isMatched: Boolean = !isUnmatched
  def isUnmatched: Boolean = vector.exists(_.isUnmatched)
}

object RegexMatchSequence {
  sealed trait Slot {
    def text: String
    def isMatched: Boolean
    def isUnmatched = !isMatched
  }
  object Slot {
    case class Matched(text: String, regex: Regex) extends Slot {
      def isMatched = true
    }
    case class Unmatched(text: String) extends Slot {
      def isMatched = false
    }
  }

  def create(regex: Regex, p: String): RegexMatchSequence = {
    val matches = regex.findAllMatchIn(p).toList

    case class Z(xs: Vector[Slot] = Vector.empty, index: Int = 0) {
      def r = {
        val a = if (index < p.length)
          xs :+ Slot.Unmatched(p.substring(index))
        else
          xs
        RegexMatchSequence(a)
      }

      def +(rhs: Match) = {
        val unmatched = if (rhs.start > index)
          Vector(Slot.Unmatched(p.substring(index, rhs.start)))
        else
          Vector.empty
        val matched = Vector(Slot.Matched(rhs.matched, regex))
        copy(xs ++ unmatched ++ matched, rhs.end)
      }
    }
    matches./:(Z())(_+_).r
  }

  def createUrl(p: String): RegexMatchSequence =
    create(RegexUtils.pattern.url, p)
}
