package org.goldenport.cli.spec

import org.goldenport.RAISE
import org.goldenport.value._
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}

/*
 * @since   Oct.  6, 2018
 * @version Oct.  8, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  kind: Parameter.Kind
) {
  def parse(req: CliRequest, args: List[String]): Option[(CliRequest, List[String])] =
    kind match {
      case Parameter.SwitchKind => args match {
        case x :: Nil if is_match(x) => Some((req.add(CliSwitch(x, None)), Nil))
        case x :: xs if is_match(x) => Some((req.add(CliSwitch(x, None)), xs))
        case _ => None
      }
      case Parameter.PropertyKind => args match {
        case x :: Nil if is_match(x) => RAISE.invalidArgumentFault(x)
        case x :: xx :: xs if is_match(x) => Some((req.add(CliProperty(x, CliArgument(xx, None))), xs))
        case _ => None
      }
    }

  protected def is_match(p: String): Boolean =
    if (p.startsWith("--"))
      p.substring(2) == name
    else if (p.startsWith("-"))
      p.substring(1) == name
    else
      false
}

object Parameter {
  sealed trait Kind extends NamedValueInstance
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(SwitchKind, PropertyKind)
  }
  case object SwitchKind extends Kind {
    val name = "switch"
  }
  case object PropertyKind extends Kind {
    val name = "property"
  }
}
