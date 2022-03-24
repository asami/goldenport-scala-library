package org.goldenport.trace

import org.goldenport.extension.Showable

/*
 * @since   Feb. 25, 2021
 *  version Mar. 28, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
case class TraceHandle(ctx: TraceContext) extends Showable {
  def print = ctx.print
  def display = ctx.display
  def show = ctx.show
  override def embed = ctx.embed
}

object TraceHandle {
  def create() = TraceContext.create().toHandle
}
