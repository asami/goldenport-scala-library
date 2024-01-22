package org.goldenport.context

import org.goldenport.util.StringUtils

/*
 * @since   Dec. 20, 2021
 *  version Mar. 19, 2022
 * @version Aug. 21, 2023
 * @author  ASAMI, Tomoharu
 */
trait Showable extends org.goldenport.extension.Showable {
  def display = print
  def show = print
}

object Showable {
  trait Base extends Showable {
    def name: String
    protected lazy val label_string = StringUtils.capitalize(name)

    def print = s"[${label_string}]${print_String}"
    override def display = s"[${label_string}]${display_String}"
    override def show = s"[${label_string}]${show_String}"

    protected def print_String: String

    protected def display_String: String

    protected def show_String: String
  }
}
