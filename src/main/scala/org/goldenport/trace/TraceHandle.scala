package org.goldenport.trace

import org.goldenport.extension.Showable

/*
 * @since   Feb. 25, 2021
 * @version Feb. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class TraceHandle(ctx: TraceContext) extends Showable {
  def print = toString
  def display = print
  def show = print
  def embed = print
}

object TraceHandle {
  val empty = TraceContext.empty.toHandle
}
