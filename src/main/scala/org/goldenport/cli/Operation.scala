package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 * @version Feb. 21, 2019
 * @author  ASAMI, Tomoharu
 */
trait Operation {
  def apply(env: Environment, req: Request): Response
}
