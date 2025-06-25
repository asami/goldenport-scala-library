package org.goldenport.datatype

import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NString
import org.goldenport.util.StringUtils

/*
 * @since   Jun. 24, 2025
 * @version Jun. 24, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class I18NTitle() extends Datatype {
  import I18NTitle._

  protected def print_String = title.en

  def title: I18NString

  protected def title_Min: Int = TITLE_MIN
  protected def title_Max: Int = TITLE_MAX
  protected def is_Valid(p: I18NString): Boolean = isTitleString(p)

  require (title.minLength >= title_Min, s"Too short: ${title.minLength}")
  require (title.maxLength <= title_Max, s"Too large: ${title.maxLength}")
  require (is_Valid(title), s"Invalid title")

  override def toString() = title.toString
}

object I18NTitle {
  val TITLE_MIN = 1
  val TITLE_MAX = 128

  def apply(title: String): I18NTitle = Plain(I18NString(title))

  def parse(title: String): Consequence[I18NTitle] = ???

  def isTitleString(s: I18NString): Boolean = ???

  case class Plain(title: I18NString) extends I18NTitle
}
