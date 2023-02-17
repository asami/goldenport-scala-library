package org.goldenport.cli.spec

import org.goldenport.RAISE
import org.goldenport.value._
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}
import org.goldenport.util.AnyUtils

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 *  version Feb. 16, 2020
 * @version Jan. 30, 2023
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  kind: Parameter.Kind,
  datatype: Parameter.DataType
) {
  def parse(req: CliRequest, args: Seq[String]): Option[(CliRequest, List[String])] =
    kind match {
      case Parameter.SwitchKind => args.toList match {
        case x :: Nil if is_switch(x) => Some((req.add(CliSwitch(switch_name(x), None)), Nil))
        case x :: xs if is_switch(x) => Some((req.add(CliSwitch(switch_name(x), None)), xs))
        case _ => None
      }
      case Parameter.PropertyKind => args.toList match {
        case x :: Nil if is_switch(x) => RAISE.invalidArgumentFault(switch_name(x))
        case x :: xx :: xs if is_switch(x) => Some((req.add(CliProperty(switch_name(x), CliArgument(xx, None))), xs))
        case _ => None
      }
    }

  protected def is_switch(p: String): Boolean =
    if (p.startsWith("--"))
      p.substring(2) == name
    else if (p.startsWith("-"))
      p.substring(1) == name
    else
      false

  protected def switch_name(p: String) = 
    if (p.startsWith("--"))
      p.substring(2)
    else if (p.startsWith("-"))
      p.substring(1)
    else
      p

  def build(req: CliRequest, args: Seq[Any]): (CliRequest, List[Any]) =
    kind match {
      case Parameter.SwitchKind => args.toList match {
        case x :: Nil if is_switch(x) => (req.add(CliSwitch(_to_name(x), None)), Nil)
        case x :: xs if is_switch(x) => (req.add(CliSwitch(_to_name(x), None)), xs)
        case _ => (req, args.toList)
      }
      case Parameter.PropertyKind => args.toList match {
        case x :: Nil if is_switch(x) => RAISE.invalidArgumentFault(s"$x")
        case x :: xx :: xs if is_switch(x) => (req.add(CliProperty(_to_name(x), CliArgument(datatype.toInstance(xx), None))), xs)
        case _ => (req, args.toList)
      }
    }

  protected def _to_name(p: Any) = switch_name(p.toString)

  protected def is_switch(p: Any): Boolean = p match {
    case m: String => is_switch(m)
    case _ => false
  }
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

  sealed trait DataType extends NamedValueInstance {
    type InstanceType
    def toInstance(p: Any): InstanceType
  }
  object DataType extends EnumerationClass[DataType] {
    val elements = Vector(XString)
  }
  case object XString extends DataType {
    type InstanceType = String
    val name = "string"

    def toInstance(p: Any): String = AnyUtils.toString(p)
  }
  trait ExtensionDataType extends DataType {
  }

  def property(name: String): Parameter = Parameter(name, PropertyKind, XString)
}
