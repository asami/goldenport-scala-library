package org.goldenport.parser

import scalaz._, Scalaz._  
import org.goldenport.RAISE
import org.goldenport.util.VectorUtils
  
/*
 * @since   Aug. 21, 2018
 *  version Sep. 22, 2018
 *  version Oct.  9, 2018
 * @version Dec. 16, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseEvent {
  def getLogicalLineText: Option[String] = None
}

case class CharEvent(
  c: Char,
  next: Option[Char],
  next2: Option[Char],
  location: ParseLocation
) extends ParseEvent {
  def isMatch(p: String): Boolean = p.length match {
    case 0 => false
    case 1 => p(0) == c
    case 2 => p(0) == c && Some(p(1)) == next
    case 3 => p(0) == c && Some(p(1)) == next && Some(p(2)) == next2
    case _ => RAISE.noReachDefect
  }
}
object CharEvent {
  def apply(c: Char): CharEvent = CharEvent(c, None, None, ParseLocation.empty)

  def apply(c: Int): CharEvent = CharEvent(c.toChar, None, None, ParseLocation.empty)

  def make(p: String): Vector[CharEvent] = make(p.toVector)

  def make(ps: Seq[Char]): Vector[CharEvent] = {
    case class Z(
      r: Vector[CharEvent] = Vector.empty,
      line: Int = 1,
      offset: Int = 1,
      aftercr: Boolean = false
    ) {
      def +(rhs: Seq[Char]) = {
        // println(s"+:$rhs")
        val c = rhs(0)
        val nextc = rhs.lift(1)
        val next2c = rhs.lift(2)
        if (aftercr) {
          if (c == '\n')
            _next_line(c, nextc, next2c)
          else if (c == '\r')
            _next_line(c, nextc, next2c)
          else
            _add_after_cr(c, nextc, next2c)
        } else {
          if (c == '\n')
            _next_line(c, nextc, next2c)
          else if (c == '\r')
            _same_line(c, nextc, next2c)
          else
            _same_line(c, nextc, next2c)
        }
      }

      private def _same_line(c: Char, nextc: Option[Char], next2c: Option[Char]) =
        _add(c, nextc, next2c, line, offset + 1)

      private def _next_line(c: Char, nextc: Option[Char], next2c: Option[Char]) =
        _add(c, nextc, next2c, line + 1, 1)

      private def _add(c: Char, nextc: Option[Char], next2c: Option[Char], l: Int, o: Int) = {
        val evt = CharEvent(c, nextc, next2c, ParseLocation(line, offset))
        Z(r = r :+ evt, l, o, c == '\r')
      }

      private def _add_after_cr(c: Char, nextc: Option[Char], next2c: Option[Char]) = {
        val l = line + 1
        val o = 1
        val evt = CharEvent(c, nextc, next2c, ParseLocation(line + 1, o))
        Z(r = r :+ evt, l, o + 1, c == '\r')
      }
    }
    VectorUtils.sliding3(ps)./:(Z())(_+_).r
  }
}

case class LogicalLineEvent(
  line: LogicalLine
) extends ParseEvent {
  def location = line.location
  def getSectionTitle = line.getSectionTitle
  def getSectionUnderline = line.getSectionUnderline
  def isEmptyLine = line.isEmptyLine
  override def getLogicalLineText = Some(line.text)
}
object LogicalLineEvent {
  def make(p: LogicalLines): Vector[LogicalLineEvent] = p.lines.map(LogicalLineEvent.apply)
}

case object StartEvent extends ParseEvent
case object EndEvent extends ParseEvent

case class LineEndEvent() extends ParseEvent
