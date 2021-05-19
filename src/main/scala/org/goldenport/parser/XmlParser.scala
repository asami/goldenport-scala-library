package org.goldenport.parser

import scala.xml._
import org.goldenport.exception.RAISE
import org.goldenport.xml.dom.DomParser
import LogicalTokens._

/*
 * @since   Aug. 19, 2018
 *  version Sep. 20, 2018
 *  version Jul. 16, 2019
 *  version Oct. 12, 2019
 * @version Jan. 23, 2021
 * @author  ASAMI, Tomoharu
 */
case class XmlParser() extends Parser with LogicalTokens.ComplexTokenizer {
  def accept(context: Context, parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] = evt.c match {
    case '<' => Some(XmlParser.XmlState.init(parent, evt))
    case _ => None
  }
}

object XmlParser {
  implicit class XmlParser(val sc: StringContext) extends AnyVal {
    def xml(args: Any*): Node = RAISE.notImplementedYetDefect
  }

  case class XmlToken(
    text: String,
    location: Option[ParseLocation]
  ) extends ExternalLogicalToken {
    def raw = text
    def value = dom
    def clearLocation: LogicalToken = copy(location = None)
    val xml = XML.loadString(text) // Scala XML
    val dom = DomParser.parse(text)
  }
  object XmlToken {
    // def apply(xml: Node, location: ParseLocation): XmlToken =
    //   XmlToken(xml, Some(location))
    def apply(p: String, location: ParseLocation): XmlToken =
      XmlToken(p, Some(location))
  }

  case class XmlState(
    parent: LogicalTokensParseState,
    location: Option[ParseLocation]
  ) extends LogicalTokensParseState {
    override def addChildState(context: Context, ps: Vector[Char]): LogicalTokensParseState = {
      val text = ps.mkString
      // println(s"XML: $text")
      parent.addChildState(context, XmlToken(text, location))
    }
  }
  object XmlState {
    def init(p: LogicalTokensParseState, evt: CharEvent): LogicalTokensParseState =
      XmlOpenState(XmlState(p, Some(evt.location)))
  }

  case class XmlOpenState(
    parent: LogicalTokensParseState,
    tag: Vector[Char] = Vector.empty
  ) extends LogicalTokensParseState {
    lazy val tagName = tag.takeWhile(_not_delimiterp).mkString
    lazy val tagString = s"<${tag.mkString}>"

    private def _not_delimiterp(p: Char) = !_delimiterp(p)

    private def _delimiterp(p: Char) = p match {
      case ' ' => true
      case _ => false
    }

    override def addChildState(context: Context, ps: Vector[Char]): LogicalTokensParseState =
      copy(tag = tag ++ ps)

    override protected def handle_End(context: Context): Transition = RAISE.notImplementedYetDefect // TODO error

    override def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      evt.c match {
        case '>' => XmlTextState(this, Vector.empty)
        case '/' => evt.next match {
          case Some('>') => SkipState(
            parent.addChildState(context, ('<' +: tag) :+ '/' :+ '>')
          )
          case _ => RAISE.notImplementedYetDefect // TODO error
        }
        case m => copy(tag = tag :+ m)
      }
  }
  object XmlOpenState {
    def apply(parent: LogicalTokensParseState): XmlOpenState = XmlOpenState(parent, Vector.empty) // ('<'))
  }

  case class XmlCloseState(
    text: XmlTextState,
    tag: Vector[Char] = Vector.empty
  ) extends LogicalTokensParseState {
    lazy val tagName = tag.mkString
    lazy val tagString = s"</${tagName}>"

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      evt.c match {
        case '>' =>
          if (tagName == text.open.tagName)
            text.open.parent.addChildState(context, s"""${text.open.tagString}${text.textString}${tagString}""".toVector)
          else
            RAISE.notImplementedYetDefect // TODO error
        case '/' => this
        case c => copy(tag = tag :+ c)
      }
  }

  case class XmlTextState(
    open: XmlOpenState,
    text: Vector[Char]
  ) extends LogicalTokensParseState {
    override val use_Tokenizer = false

    lazy val textString = text.mkString

    override def addChildState(context: Context, ps: Vector[Char]) = copy(text = text ++ ps)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      evt.c match {
        case '<' => evt.next match {
          case Some(nc) if nc == '/' => XmlCloseState(this)
          case _ => XmlOpenState(this)
        }
        case c => copy(text = text :+ c)
      }
  }
}
