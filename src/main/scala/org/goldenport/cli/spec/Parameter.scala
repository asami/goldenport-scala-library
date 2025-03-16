package org.goldenport.cli.spec

import scalaz._, Scalaz._
import java.io.File
import java.net.URL
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.value._
import org.goldenport.hocon.HoconUtils
import org.goldenport.io.InputSource
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}
import org.goldenport.util.MagicSequence

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 *  version Feb. 16, 2020
 *  version Jan. 30, 2023
 * @version Mar. 17, 2025
 * @author  ASAMI, Tomoharu
 */
case class Parameter(
  name: String,
  kind: Parameter.Kind,
  datatype: DataType = XString,
  multiplicity: Multiplicity = Multiplicity.One,
  defaut: Parameter.Default = Parameter.Default.Undefined,
  isMagicSequence: Boolean = true,
  isEagerSequence: Boolean = false
) {
  import Parameter._

  def parse(req: CliRequest, args: Seq[String]): Option[(CliRequest, List[String])] =
    kind match {
      case Parameter.ArgumentKind => None
      case Parameter.SwitchKind => args.toList match {
        case x :: Nil if is_switch(x) => Some((req.add(CliSwitch(switch_name(x), true, Some(this))), Nil))
        case x :: xs if is_switch(x) => Some((req.add(CliSwitch(switch_name(x), true, Some(this))), xs))
        case _ => None
      }
      case Parameter.PropertyKind => args.toList match {
        case x :: Nil if is_switch(x) => RAISE.invalidArgumentFault(switch_name(x))
        case x :: xx :: xs if is_switch(x) => Some(
          (req.add(CliProperty(switch_name(x), datatype.toInstance(xx), Some(this))), xs)
        )
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

  def build0(req: CliRequest, args: Seq[Any]): (CliRequest, List[Any]) =
    kind match {
      case Parameter.ArgumentKind => (req, args.toList)
      case Parameter.SwitchKind => args.toList match {
        case x :: Nil if is_switch(x) => (req.add(CliSwitch(_to_name(x), true, Some(this))), Nil)
        case x :: xs if is_switch(x) => (req.add(CliSwitch(_to_name(x), true, Some(this))), xs)
        case _ => (req, args.toList)
      }
      case Parameter.PropertyKind => args.toList match {
        case x :: Nil if is_switch(x) => RAISE.invalidArgumentFault(s"$x")
        case x :: xx :: xs if is_switch(x) => (req.add(CliProperty(_to_name(x), datatype.toInstance(xx), Some(this))), xs)
        case _ => (req, args.toList)
      }
    }

  def parsePropertySwitch(p: ParseState): ParseState =
    kind match {
      case Parameter.ArgumentKind => _parse_argument(p)
      case Parameter.SwitchKind => _parse_switch(p)
      case Parameter.PropertyKind => _parse_property(p)
    }

  private def _parse_argument(p: ParseState): ParseState = {
    _parse_property(p)
  }

  private def _parse_switch(p: ParseState): ParseState = {
    case class Z(
      request: CliRequest = p.req,
      remainder: Vector[Any] = Vector.empty,
      faults: Faults = Faults.empty
    ) {
      def r = ParseState(request, remainder, p.faults + faults)

      def +(rhs: Any) = rhs match {
        case m: String =>
          if (is_switch(m)) {
            if (switch_name(m) == name)
              copy(request = request.add(CliSwitch(name, Parameter.this)))
            else
              copy(remainder = remainder :+ m)
          } else {
            copy(remainder = remainder :+ m)
          }
        case m => copy(remainder = remainder :+ m)
      }
    }
    p.args./:(Z())(_+_).r
  }

  private def _parse_property(p: ParseState): ParseState = {
    case class Z(
      request: CliRequest = p.req,
      propertyName: Option[String] = None,
      values: Vector[Any] = Vector.empty,
      remainder: Vector[Any] = Vector.empty,
      faults: Faults = Faults.empty
    ) {
      def r = propertyName match {
        case None => ParseState(request, remainder, p.faults + faults)
        case Some(name) => multiplicity match {
          case Multiplicity.One =>
            _result_missing(name)
          case Multiplicity.ZeroOne =>
            _result_skip
          case Multiplicity.OneMore =>
            if (values.isEmpty)
              _result_missing(name)
            else
              _result_more(name)
          case Multiplicity.ZeroMore => _result_more(name)
        }
      }

      private def _result_skip =
        ParseState(request, remainder, p.faults + faults)

      private def _result_missing(name: String) =
        ParseState(request, remainder, p.faults + faults :+ MissingArgumentFault(name))

      private def _result_more(name: String) = {
        val req = request.add(CliProperty(name, values, Parameter.this))
        ParseState(req, remainder, p.faults + faults)
      }

      def +(rhs: Any) = rhs match {
        case m: String =>
          if (is_switch(m)) {
            propertyName match {
              case None => _property_switch(m)
              case Some(name) => multiplicity match {
                case Multiplicity.One =>
                  values.length match {
                    case 0 =>
                      copy(
                        faults = faults :+ MissingArgumentFault(name),
                        values = Vector.empty
                      )._property_switch(m)
                    case 1 =>
                      copy(
                        request = request.add(CliProperty(name, values(0), Parameter.this)),
                        values = Vector.empty
                      )._property_switch(m)
                    case n =>
                      copy(
                        faults = faults :+ TooManyArgumentsFault(name),
                        values = Vector.empty
                      )._property_switch(m)
                  }
                case Multiplicity.ZeroOne =>
                  RAISE.notImplementedYetDefect
                case Multiplicity.OneMore =>
                  RAISE.notImplementedYetDefect
                case Multiplicity.ZeroMore =>
                  RAISE.notImplementedYetDefect
              }
            }
          } else {
            _property_value(rhs)
          }
        case m => _property_value(rhs)
      }

      private def _property_switch(p: String) =
        if (name == switch_name(p))
          copy(propertyName = Some(name))
        else
          copy(remainder = remainder :+ p)

      private def _property_value(p: Any) =
        propertyName match {
          case None => copy(remainder = remainder :+ p)
          case Some(name) =>
            if (isEagerSequence)
              multiplicity match {
                case Multiplicity.One => _property_value_one(p)
                case Multiplicity.ZeroOne => _property_value_one(p)
                case Multiplicity.OneMore => _property_value_more(p)
                case Multiplicity.ZeroMore => _property_value_more(p)
              }
            else
              _property_value_one(p)
        }

      private def _property_value_one(p: Any) = 
        datatype.cInstance(p).fold(
          c => copy(faults = faults :+ ValueDomainDatatypeFault(name, datatype, p)),
          x => copy(
            request = request.add(CliProperty(name, p, Parameter.this)),
            propertyName = None
          )
        )

      private def _property_value_more(p: Any) =
        datatype.cInstance(p).fold(
          c => copy(faults = faults :+ ValueDomainDatatypeFault(name, datatype, p)),
          x => copy(values = values ++ _magic_sequence(p)))

      private def _magic_sequence(p: Any): Vector[Any] =
        if (isMagicSequence)
          p match {
            case s: String => MagicSequence.toVector(s)
            case m => Vector(m)
          }
        else
          Vector(p)
    }
    p.args./:(Z())(_+_).r
  }

  def parseArgument(p: ParseState): ParseState = kind match {
    case Parameter.ArgumentKind => multiplicity match {
      case Multiplicity.One =>
        p.args.headOption match {
          case None => p.addFault(MissingArgumentFault()) // TODO
          case Some(s) => ParseState(p.req.add(CliArgument(name, s, Parameter.this)), p.args.tail)
        }
      case Multiplicity.ZeroOne =>
        RAISE.notImplementedYetDefect
      case Multiplicity.OneMore =>
        RAISE.notImplementedYetDefect
      case Multiplicity.ZeroMore =>
        RAISE.notImplementedYetDefect
    }
    case _ => p
  }

 protected def _to_name(p: Any) = switch_name(p.toString)

  protected def is_switch(p: Any): Boolean = p match {
    case m: String => is_switch(m)
    case _ => false
  }

  def cFile(p: Any): Consequence[File] =
    for {
      a <- datatype.cInstance(p)
      r <- a match {
        case m: File => Consequence.success(m)
        case m => Consequence.valueDomainFault(name, datatype, p)
      }
    } yield r

  def cInputSource(p: Any): Consequence[InputSource] =
    p match {
      case m: InputSource => Consequence.success(m)
      case m: File => Consequence.success(InputSource(m))
      case m: URL => Consequence.success(InputSource(m))
      case m: String => Consequence(InputSource.create(m))
    }

  def cInputSourceList(ps: Seq[Any]): Consequence[List[InputSource]] =
    ps.toList.traverse(cInputSource)

  def cConfig(p: Any): Consequence[Hocon] =
    for {
      in <- cInputSource(p)
      s <- Consequence(in.asText)
      c <- HoconUtils.parseConfig(s)
    } yield c
}

object Parameter {
  sealed trait Kind extends NamedValueInstance
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(SwitchKind, PropertyKind)
  }
  case object ArgumentKind extends Kind {
    val name = "argument"
  }
  case object SwitchKind extends Kind {
    val name = "switch"
  }
  case object PropertyKind extends Kind {
    val name = "property"
  }

  sealed trait Default extends NamedValueInstance
  object Default extends EnumerationClass[Default] {
    val elements = Vector()

    case object Undefined extends Default {
      val name = "undefined"
    }
    case object Empty extends Default {
      val name = "empty"
    }
  }

  case class ParseState(
    req: CliRequest,
    args: Vector[Any],
    faults: Faults = Faults.empty
  ) {
    def addFault(p: Fault) = copy(faults = faults :+ p)

    def RAISE_IF_FAILURE: Unit = faults.RAISE_IF_FAILURE
  }
  object ParseState {
    def create(name: String, ps: Seq[Any]): ParseState = ParseState(
      CliRequest(name),
      ps.toVector
    )
  }

  def argument(name: String): Parameter = Parameter(name, ArgumentKind)

  def argumentFile(name: String): Parameter = Parameter(name, ArgumentKind, XFile)

  def property(name: String): Parameter = Parameter(name, PropertyKind)

  def propertyConfigFileOrEmpty(): Parameter = propertyConfigFileOrEmpty("config")

  def propertyConfigFileOrEmpty(name: String): Parameter =
    Parameter(name, PropertyKind, XConfig, Multiplicity.One, Default.Empty)

  def propertyInputSourceSequence(name: String): Parameter =
    Parameter(name, PropertyKind, XInputSource, Multiplicity.ZeroMore)
}
