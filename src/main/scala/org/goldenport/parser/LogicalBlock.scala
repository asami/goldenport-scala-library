package org.goldenport.parser

import org.goldenport.RAISE
import org.goldenport.i18n.I18NElement

/*
 * @since   Sep. 22, 2018
 * @version Oct. 27, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalBlock {
  def isEmpty: Boolean
  def lines: LogicalLines
  def getText: Option[String] // XXX title and body
}

object LogicalBlock {
  val empty = LogicalParagraph(LogicalLines.empty)

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

  def :+(rhs: LogicalBlock): LogicalSection = copy(blocks = blocks :+ rhs)
  def :+(rhs: LogicalBlocks): LogicalSection = copy(blocks = blocks + rhs)

  def getText = Some(blocks.text)
}
object LogicalSection {
  def apply(title: String, blocks: LogicalBlocks): LogicalSection =
    LogicalSection(I18NElement(title), blocks)

  def create(title: String, content: String): LogicalSection =
    LogicalSection(I18NElement(title), LogicalBlocks.create(content))
}

case class LogicalParagraph(
  lines: LogicalLines,
  location: Option[ParseLocation] = None
) extends LogicalBlock {
  def isEmpty = lines.isEmpty

  def getText = Some(lines.text)
}
object LogicalParagraph {
  def apply(p: String): LogicalParagraph = LogicalParagraph(LogicalLines(p))

  def apply(ps: Seq[LogicalLine]): LogicalParagraph = LogicalParagraph(LogicalLines(ps.toVector))
}
