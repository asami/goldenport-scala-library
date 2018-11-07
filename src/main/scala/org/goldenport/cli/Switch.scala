package org.goldenport.cli

import org.goldenport.cli.spec.{Parameter => SpecParameter}

/*
 * @since   Oct.  5, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Switch(
  name: String,
  spec: Option[SpecParameter]
) {
}

object Switch {
  def get(p: String): Option[Switch] =
    if (p.startsWith("-"))
      Some(Switch(p.dropWhile(_ == '-'), None))
    else
      None
}
