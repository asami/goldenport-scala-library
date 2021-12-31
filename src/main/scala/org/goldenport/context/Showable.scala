package org.goldenport.context

/*
 * @since   Dec. 20, 2021
 * @version Dec. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait Showable extends org.goldenport.extension.Showable {
  def display = print
  def show = print
  def embed = print
}
