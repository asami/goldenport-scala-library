package org.goldenport.context

/*
 * @since   Dec. 20, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
trait Showable extends org.goldenport.extension.Showable {
  def display = print
  def show = print
}
