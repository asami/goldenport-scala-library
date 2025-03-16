package org.goldenport.cli

import java.net.URL
import org.goldenport.cli.spec.{Parameter => SpecParameter}

/*
 * @since   Oct.  5, 2018
 *  version Oct.  8, 2018
 *  version Apr. 25, 2021
 * @version Mar. 16, 2025
 * @author  ASAMI, Tomoharu
 */
case class Property(
  name: String,
  value: Any,
  spec: Option[SpecParameter]
) extends Parameter {
  def isMatch(p: SpecParameter): Boolean = p.kind match {
    case SpecParameter.ArgumentKind => false
    case SpecParameter.SwitchKind => false
    case SpecParameter.PropertyKind => p.name == name
  }
}

object Property {
  def apply(
    name: String,
    value: Any,
    spec: SpecParameter
  ): Property = Property(name, value, Some(spec))
}
