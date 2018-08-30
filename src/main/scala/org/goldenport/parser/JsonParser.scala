package org.goldenport.parser

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import play.api.libs.json._
import org.goldenport.exception.RAISE
import LogicalTokens._

/*
 * @since   Aug. 19, 2018
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class JsonParser() extends Parser with LogicalTokens.ComplexTokenizer {
  def accept(parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] = evt.c match {
    case '{' => Some(JsonParser.JsonState(parent))
    case _ => None
  }
}

object JsonParser {
  implicit class JsonInterpolator(val sc: StringContext) extends AnyVal {
    def json(args: Any*): JsValue = RAISE.notImplementedYetDefect
  }

  // def parseLiteral(p: String): (Option[JsValue], Vector[CharEvent]) =
  //   parseLiteral(p.toVector)

  // def parseLiteral(ps: Vector[Char]): (Option[JsValue], Vector[CharEvent]) =
  //   _parse_literal(CharEvent.make(ps))

  // private def _parse_literal(p: Vector[ParseEvent]): (Option[JsValue], Vector[CharEvent]) = {
  //   RAISE.notImplementedYetDefect
  // }

  case class JsonToken(json: JsValue) extends ExternalLogicalToken

  case class JsonState(
    parent: LogicalTokensParseState,
    count: Int,
    text: Vector[Char]
  ) extends LogicalTokensParseState {
    override def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState = {
      p.tokens./:(this) { (z, x) =>
        x match {
          case m: StringToken => z.copy(text = text ++ _to_string(m))
          case m => RAISE.noReachDefect(s"JsonState#addChildState: $m")
        }
      }
    }

    private def _to_string(p: StringToken) = '"' +: p.text.toVector :+ '"'

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      val c = evt.c
      c match {
        case '{' => copy(count = count + 1, text = text :+ c)
        case '}' =>
          count - 1 match {
            case 0 => parent.addChildState(config, _to_token(text :+ c))
            case n => copy(count = n, text = text :+ c)
          }
        case _ => copy(text = text :+ c)
      }
    }

    private def _to_token(p: Vector[Char]): JsonToken = _to_token(p.mkString)
    private def _to_token(p: String): JsonToken = try {
      JsonToken(Json.parse(p))
    } catch {
      case NonFatal(e) => RAISE.noReachDefect(s"JSON: $p")
    }
  }
  object JsonState {
    def apply(parent: LogicalTokensParseState): JsonState = JsonState(parent, 1, Vector('{'))
  }
}
