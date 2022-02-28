package org.goldenport.extension

import org.goldenport.util.StringUtils

/*
 * @since   Jul. 24, 2019
 *  version Sep. 18, 2019
 *  version Oct. 16, 2019
 * @version Feb. 23, 2022
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
   * Sufficient short information for debug.
   */
  def show: String

  /*
   * Minimal information for embedding.
   */
  def embed: String
  def embed(width: Int): String = StringUtils.toEmbedConsole(embed, width)
}
