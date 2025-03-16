package org.goldenport.cli

import org.goldenport.cli.spec.{Parameter => SpecParameter}

/*
 * @since   Oct.  5, 2018
 *  version Oct. 10, 2018
 * @version Mar. 16, 2025
 * @author  ASAMI, Tomoharu
 */
case class Switch(
  name: String,
  value: Boolean,
  spec: Option[SpecParameter]
) extends Parameter {
  def isMatch(p: SpecParameter): Boolean = p.kind match {
    case SpecParameter.ArgumentKind => false
    case SpecParameter.SwitchKind => p.name == name
    case SpecParameter.PropertyKind => false
  }
}

object Switch {
  def apply(name: String, spec: SpecParameter): Switch = Switch(name, true, Some(spec))

  def get(p: String): Option[Switch] =
    if (p.startsWith("-"))
      Some(Switch(p.dropWhile(_ == '-'), true, None))
    else
      None
}
