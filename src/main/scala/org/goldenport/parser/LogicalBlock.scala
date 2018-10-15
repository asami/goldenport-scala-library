package org.goldenport.parser

import org.goldenport.RAISE
import org.goldenport.i18n.I18NElement

/*
 * @since   Sep. 22, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait LogicalBlock {
  def isEmpty: Boolean
  def lines: LogicalLines
}

object LogicalBlock {
  val empty = LogicalParagraph(LogicalLines.empty)

  def apply(p: LogicalLine, ps: LogicalLine*): LogicalBlock = apply(p +: ps)

  def apply(ps: Seq[LogicalLine]): LogicalBlock = LogicalParagraph(LogicalLines(ps))

  def create(p: String): LogicalBlock = apply(LogicalLine(p))
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
}
object LogicalParagraph {
  def apply(p: String): LogicalParagraph = LogicalParagraph(LogicalLines(p))

  def apply(ps: Seq[LogicalLine]): LogicalParagraph = LogicalParagraph(LogicalLines(ps.toVector))
}
