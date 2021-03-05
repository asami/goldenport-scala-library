package org.goldenport.i18n

import org.goldenport.util.StringUtils

/*
 * @since   Jan.  2, 2021
 * @version Jan.  2, 2021
 * @author  ASAMI, Tomoharu
 */
case class StringFormatter() {
  def isWordSeparating(prev: Char, next: Char): Boolean = StringFormatter.isWordSeparating(prev, next)
}

object StringFormatter {
  val default = StringFormatter()

  def isWordSeparating(prev: Char, next: Char): Boolean =
    StringUtils.isAsciiChar(prev) && StringUtils.isAsciiChar(next)
}
