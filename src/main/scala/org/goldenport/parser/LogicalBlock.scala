package org.goldenport.parser

import org.goldenport.RAISE
import org.goldenport.i18n.I18NElement

/*
 * @since   Sep. 22, 2018
 *  version Oct. 27, 2018
 *  version Jan. 20, 2019
 *  version Feb.  9, 2019
 * @version May. 19, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalBlock {
  def isEmpty: Boolean
  def lines: LogicalLines
  def getText: Option[String] // XXX title and body
}

object LogicalBlock {
  val empty = LogicalParagraph(LogicalLines.empty)

  trait VerbatimMarkClass {
    def get(p: LogicalLine): Option[VerbatimMark]
  }
  trait VerbatimMark {
    def isDone(p: LogicalLine): Boolean
  }
  object RawBackquoteMarkClass extends VerbatimMarkClass {
    def isMatch(p: LogicalLine) = p.text == "```" || p.text.startsWith("``` ")
    def get(p: LogicalLine): Option[VerbatimMark] =
      if (isMatch(p))
        Some(RawBackquoteMark(p))
      else
        None
  }
  case class RawBackquoteMark(line: LogicalLine) extends VerbatimMark {
    def isDone(p: LogicalLine): Boolean = RawBackquoteMarkClass.isMatch(p)
  }

  def apply(p: LogicalLine, ps: LogicalLine*): LogicalBlock = apply(p +: ps)

  def apply(ps: Seq[LogicalLine]): LogicalBlock = LogicalParagraph(LogicalLines(ps))

  def create(p: String): LogicalBlock = apply(LogicalLine(p))
}

case object StartBlock extends LogicalBlock {
  def isEmpty = true
  def lines = LogicalLines.empty
  def getText = None
}

case object EndBlock extends LogicalBlock {
  def isEmpty = true
  def lines = LogicalLines.empty
  def getText = None
}

case class LogicalSection(
  title: I18NElement,
  blocks: LogicalBlocks,
  location: Option[ParseLocation] = None
) extends LogicalBlock {
  def isEmpty = false

  def lines: LogicalLines = blocks.lines

  def +(rhs: LogicalSection): LogicalSection = copy(blocks = blocks + rhs.blocks)

  def :+(rhs: LogicalBlock): LogicalSection = copy(blocks = blocks :+ rhs)
  def :+(rhs: LogicalBlocks): LogicalSection = copy(blocks = blocks + rhs)

  def getText = Some(blocks.text)
}
object LogicalSection {
  def apply(title: String, blocks: LogicalBlocks): LogicalSection =
    LogicalSection(I18NElement(title), blocks)

  def apply(title: String, blocks: LogicalBlocks, location: ParseLocation): LogicalSection =
    LogicalSection(I18NElement(title), blocks, Some(location))

  def create(title: String, content: String): LogicalSection =
    LogicalSection(I18NElement(title), LogicalBlocks.create(content))
}

case class LogicalParagraph(
  lines: LogicalLines
) extends LogicalBlock {
  def isEmpty = lines.isEmpty

  def getText = Some(lines.text)
}
object LogicalParagraph {
  def apply(p: String): LogicalParagraph = LogicalParagraph(LogicalLines(p))

  def apply(p: String, location: ParseLocation): LogicalParagraph = LogicalParagraph(LogicalLines(p, location))

  def apply(ps: Seq[LogicalLine]): LogicalParagraph = LogicalParagraph(LogicalLines(ps.toVector))
}

case class LogicalVerbatim(
  mark: LogicalBlock.VerbatimMark,
  lines: LogicalLines,
  location: Option[ParseLocation] = None
) extends LogicalBlock {
  def isEmpty = lines.isEmpty

  def getText = Some(lines.text)
}
object LogicalVerbatim {
}
