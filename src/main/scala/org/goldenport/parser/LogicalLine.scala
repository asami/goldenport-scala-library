package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.I18NElement

/*
 * @since   Sep. 22, 2018
 * @version Sep. 23, 2018
 * @author  ASAMI, Tomoharu
 */
case class LogicalLine(
  text: String,
  location: Option[ParseLocation] = None
) {
  import LogicalLine._
  def getSectionTitle: Option[SectionTitle] = SectionTitle.get(text)
  def getSectionUnderline: Option[SectionUnderline] = SectionUnderline.get(text)
  def isEmptyLine: Boolean = Strings.blankp(text)
}

object LogicalLine {
  val sectionOutlineMarks = Vector('*', '#')
  val sectionUnderlineMarks = Vector('=', '-')

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
  case class SectionUnderline(mark: String)
  object SectionUnderline {
    def get(p: String): Option[SectionUnderline] =
      p.headOption.filter(sectionUnderlineMarks.contains).flatMap(c =>
        if (p.trim.distinct == c.toString)
          Some(SectionUnderline(c.toString))
        else
          None
      )
  }
}
