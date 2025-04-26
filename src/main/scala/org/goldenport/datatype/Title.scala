package org.goldenport.datatype

import org.goldenport.context.Consequence
import org.goldenport.util.StringUtils

/*
 * @since   Apr. 19, 2025
 * @version Apr. 21, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class Title() extends Datatype {
  import Title._

  def title: String

  protected def title_Min: Int = TITLE_MIN
  protected def title_Max: Int = TITLE_MAX
  protected def is_Valid(p: String): Boolean = isTitleString(p)

  require (title.length >= title_Min, s"Too short: ${title.length}")
  require (title.length <= title_Max, s"Too large: ${title.length}")
  require (is_Valid(title), s"Invalid title")
}

object Title {
  val TITLE_MIN = 1
  val TITLE_MAX = 256

  def apply(title: String): Title = PlainTitle(title)

  def parse(title: String): Consequence[Title] = ???

  def isTitleChar(c: Char): Boolean =
    !(StringUtils.isAsciiChar(c) && !StringUtils.isSafeAsciiChar(c))

  def isTitleString(s: String): Boolean =
    s.length > 0 && s.forall(isTitleChar)
}

case class PlainTitle(title: String) extends Title {
}

