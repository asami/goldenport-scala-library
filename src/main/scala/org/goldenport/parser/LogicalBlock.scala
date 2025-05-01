package org.goldenport.parser

import org.goldenport.RAISE
import org.goldenport.i18n.I18NElement
import org.goldenport.util.StringUtils

/*
 * @since   Sep. 22, 2018
 *  version Oct. 27, 2018
 *  version Jan. 20, 2019
 *  version Feb.  9, 2019
 *  version May. 19, 2019
 *  version Feb. 15, 2021
 *  version May. 16, 2021
 *  version Dec. 31, 2021
 *  version Nov. 27, 2022
 *  version Oct. 14, 2023
 * @version Feb.  8, 2025
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
    def isMatch(p: String): Boolean
    def get(p: LogicalLine): Option[VerbatimMark]
    def get(p: String): Option[VerbatimMark]
  }
  trait VerbatimMark {
    def isDone(p: String): Boolean
    def isDone(p: LogicalLine): Boolean
  }
  object RawBackquoteMarkClass extends VerbatimMarkClass {
    def isMatch(p: String): Boolean = p == "```" || p.startsWith("``` ")
    def isMatch(p: LogicalLine): Boolean = isMatch(p.text)
    def get(p: LogicalLine): Option[VerbatimMark] =
      if (isMatch(p.text))
        Some(RawBackquoteMark(p))
      else
        None
    def get(p: String): Option[VerbatimMark] = get(LogicalLine(p))
  }
  case class RawBackquoteMark(line: LogicalLine) extends VerbatimMark {
    def isDone(p: String): Boolean = isDone(LogicalLine(p))
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
  mark: Option[String] = None,
  location: Option[ParseLocation] = None
) extends LogicalBlock {
  override def toString(): String = {
    val s = blocks.sections.toList match {
      case Nil => blocks.blocks.toStream.flatMap {
        case StartBlock => None
        case EndBlock => None
        case m: LogicalSection => Some(m.nameForModel)
        case m: LogicalParagraph => m.getText.map(StringUtils.forToString)
        case m: LogicalVerbatim => m.getText.map(StringUtils.forToString)
      }.headOption.getOrElse("EMPTY")
      case xs => xs.map(_.nameForModel).mkString(",")
    }
    s"LogicalSection[${title.nameForModel}]: ${s}"
  }

  def isEmpty = false
  def keyForModel: String = title.keyForModel
  def nameForModel: String = title.nameForModel

  def lines: LogicalLines = blocks.lines

  def +(rhs: LogicalSection): LogicalSection = copy(blocks = blocks + rhs.blocks)

  def :+(rhs: LogicalBlock): LogicalSection = copy(blocks = blocks :+ rhs)
  def :+(rhs: LogicalBlocks): LogicalSection = copy(blocks = blocks + rhs)

  def text = blocks.text
  def getText = Some(text)

  def sections = blocks.sections

  def prologue = blocks.prologue
}
object LogicalSection {
  val empty = LogicalSection("", LogicalBlocks.empty)

  def apply(title: String, blocks: LogicalBlocks): LogicalSection =
    LogicalSection(I18NElement(title), blocks)

  def apply(title: String, blocks: LogicalBlocks, mark: String): LogicalSection =
    LogicalSection(I18NElement(title), blocks, Some(mark))

  def apply(title: String, blocks: LogicalBlocks, location: ParseLocation): LogicalSection =
    LogicalSection(I18NElement(title), blocks, None, Some(location))

  def apply(title: String, blocks: LogicalBlocks, mark: String, location: ParseLocation): LogicalSection =
    LogicalSection(I18NElement(title), blocks, Some(mark), Some(location))

  def create(title: String, content: String): LogicalSection =
    LogicalSection(I18NElement(title), LogicalBlocks.create(content))

  def createOrg(title: String, content: String): LogicalSection =
    LogicalSection(title, LogicalBlocks.create(content), "*")

  def createMd(title: String, content: String): LogicalSection =
    LogicalSection(title, LogicalBlocks.create(content), "#")
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
