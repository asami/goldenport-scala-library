package org.goldenport.i18n

/*
 * @since   Jun. 24, 2025
 * @version Jun. 24, 2025
 * @author  ASAMI, Tomoharu
 */
object I18NUtils {
  def maxLength(ps: Seq[String]): Int = ps.map(length).max
  def minLength(ps: Seq[String]): Int = ps.map(length).min
  def length(s: String): Int = s.codePointCount(0, s.length)
}
