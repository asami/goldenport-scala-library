package org.goldenport.extension

import org.goldenport.util.StringUtils

/*
 * @since   Jul. 24, 2019
 *  version Sep. 18, 2019
 *  version Oct. 16, 2019
 *  version Feb. 23, 2022
 *  version Mar. 19, 2022
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait Showable {
  /*
   * Natural representation for data. Show as-is even large data. No security blinding.
   */
  def print: String

  /*
   * 1 line representation for interaction representation (e.g. REPL). Security blinding.
   */
  def display: String

  /*
   * Sufficient short information for debug. Security blinding.
   */
  def show: String

  /*
   * Literal representation.
   */
  def getLiteral: Option[String] = None

  /*
   * Minimal information for embedding. Security blinding.
   */
  def embed: String = embed(16)
  def embed(width: Int): String = StringUtils.toEmbedConsole(display, width)
}
