package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.I18NElement

/*
 * @since   Sep. 22, 2018
 *  version Feb.  2, 2019
 *  version Feb. 13, 2021
 * @version Oct. 26, 2024
 * @author  ASAMI, Tomoharu
 */
case class LogicalLine(
  text: String,
  physicalLines: List[String],
  location: Option[ParseLocation] = None
) {
  import LogicalLine._
  def getSectionTitle: Option[SectionTitle] = SectionTitle.get(text)
  def getSectionUnderline: Option[SectionUnderline] = SectionUnderline.get(physicalLines)
  def isEmptyLine: Boolean = Strings.blankp(text)
}

object LogicalLine {
  val sectionOutlineMarks = Vector('*', '#')
  val sectionUnderlineMarks = Vector('=', '-')

  val empty = LogicalLine("")

  def apply(text: String): LogicalLine = LogicalLine(text, List(text))

  def apply(
    text: String,
    location: ParseLocation
  ): LogicalLine = LogicalLine(text, List(text), Some(location))

  def apply(
    text: String,
    location: Option[ParseLocation]
  ): LogicalLine = LogicalLine(text, List(text), location)

  def start(text: String) = LogicalLine(text, ParseLocation.start)

  case class Builder(
    cs: Vector[Char] = Vector.empty,
    location: Option[ParseLocation] = None
  ) {
    def lastOption: Option[Char] = cs.headOption
    def getCurrentLine: Option[String] = if (cs.isEmpty) None else Some(cs.mkString)
    def currentLine: String = cs.mkString

    def add(evt: CharEvent): Builder =
      if (location.isDefined)
        copy(cs = cs :+ evt.c)
      else
        copy(location = Some(evt.location), cs = cs :+ evt.c)

    def build(): Option[LogicalLine] = getCurrentLine.map(x => LogicalLine(x, location))
  }
  object Builder {
    def apply(p: String, location: Option[ParseLocation]): Builder = Builder(p.toVector, location)
  }

  // ** TITLE
  case class SectionTitle(mark: String, level: Int, title: String) {
    def toI18NElement = I18NElement(title)
  }
  object SectionTitle {
    def get(p: String): Option[SectionTitle] =
      p.headOption.filter(sectionOutlineMarks.contains).flatMap(c =>
          p.span(_ == c) match {
            case (marks, desc) if (marks.length > 0) =>
              if (desc.length > 1 && desc(0) == ' ')
                Some(SectionTitle(c.toString, marks.length, desc.tail))
              else
                None
            case _ => None
          }
      )
  }

  // TITLE
  // =====
  case class SectionUnderline(mark: String, title: String)
  object SectionUnderline {
    def get(ps: List[String]): Option[SectionUnderline] =
      ps match {
        case Nil => None
        case t :: x :: Nil => x.headOption.flatMap { c =>
          if (sectionUnderlineMarks.contains(c) && x.tail.forall(sectionUnderlineMarks.contains))
            Some(SectionUnderline(c.toString, t))
          else
            None
        }
        case _ => None
      }
  }
}
