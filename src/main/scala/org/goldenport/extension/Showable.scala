package org.goldenport.extension

/*
 * @since   Jul. 24, 2019
 * @version Jul. 26, 2019
 * @author  ASAMI, Tomoharu
 */
trait Showable {
  /*
   * Natural representation for data. Show as-is even large data.
   */
  def print: String

  /*
   * 1 line representation for interaction representation (e.g. REPL).
   */
  def display: String

  /*
   * Show a shortened natural representation with some information.
   */
  def show: String
}
