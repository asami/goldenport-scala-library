package org.goldenport.cli

import org.goldenport.cli.spec.{Parameter => SpecParameter}
import org.goldenport.bag.BufferBag
import org.goldenport.util.AnyUtils

/*
 * @since   Oct.  5, 2018
 *  version May. 19, 2019
 * @version Feb. 16, 2020
 * @author  ASAMI, Tomoharu
 */
case class Argument(
  value: Any,
  spec: Option[SpecParameter]
) {
  def asString: String = AnyUtils.toString(value)
  def toInputText: String = BufferBag.fromUri(value.toString).toText
}

object Argument {
  def apply(p: Any): Argument = Argument(p, None)
}
