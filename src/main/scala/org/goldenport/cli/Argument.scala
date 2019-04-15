package org.goldenport.cli

import org.goldenport.cli.spec.{Parameter => SpecParameter}
import org.goldenport.bag.BufferBag

/*
 * @since   Oct.  5, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Argument(
  value: Any,
  spec: Option[SpecParameter]
) {
  def toInputText: String = BufferBag.fromUri(value.toString).toText
}

object Argument {
  def apply(p: String): Argument = Argument(p, None)
}
