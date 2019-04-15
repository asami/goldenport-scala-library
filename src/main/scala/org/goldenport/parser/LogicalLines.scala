package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 20, 2018
 *  version Sep. 29, 2018
 *  version Oct. 30, 2018
 *  version Dec. 31, 2018
 *  version Jan.  1, 2019
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class LogicalLines(
  lines: Vector[LogicalLine]
) {
  def isEmpty: Boolean = lines.isEmpty

  def +(rhs: LogicalLines) = LogicalLines(lines ++ rhs.lines)
  def :+(p: LogicalLine) = LogicalLines(lines :+ p)
  def :+(p: String) = LogicalLines(lines :+ LogicalLine(p))
  def +:(p: LogicalLine) = LogicalLines(p +: lines)
  def +:(p: String) = LogicalLines(LogicalLine(p) +: lines)

  def text = lines.map(_.text + "\n").mkString
}

object LogicalLines {
  type Transition = (ParseMessageSequence, ParseResult[LogicalLines], LogicalLinesParseState)

  implicit object LogicalLinesMonoid extends Monoid[LogicalLines] {
    def zero = empty
    def append(lhs: LogicalLines, rhs: => LogicalLines) = lhs + rhs
  }

  val empty = LogicalLines(Vector.empty)

  def apply(ps: Seq[LogicalLine]): LogicalLines = LogicalLines(ps.toVector)

  def apply(p: String, ps: String*): LogicalLines = LogicalLines(LogicalLine(p) +: ps.toVector.map(LogicalLine(_)))

  def apply(p: LogicalLine, ps: LogicalLine*): LogicalLines = LogicalLines(p +: ps.toVector)

  def apply(p: String, location: ParseLocation): LogicalLines = LogicalLines(Vector(LogicalLine(p, location)))

  def start(p: String, ps: String*): LogicalLines = start(1, p +: ps)

  def start(i: Int, p: String, ps: String*): LogicalLines = start(i, p +: ps)

  def start(i: Int, ps: Seq[String]): LogicalLines = {
    val a = ps.zipWithIndex.map {
      case (x, y) => LogicalLine(x, ParseLocation(y + i, 1))
    }
    LogicalLines(a.toVector)
  }

  def parse(in: String): LogicalLines = parse(Config.default, in.toVector)

  def parse(conf: Config, in: String): LogicalLines = parse(conf, in.toVector)

  def parse(conf: Config, in: Seq[Char]): LogicalLines = {
    val parser = ParseReaderWriterStateClass(conf, InitState)
    val (messages, result, state) = parser.apply(in)
    result match {
      case ParseSuccess(ast, _) => ast.concatenate
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  case class Config(
    useDoubleQuote: Boolean = true,
    useSingleQuote: Boolean = true,
    useAngleBracket: Boolean = true, // XML
    useBrace: Boolean = true, // JSON
    useParenthesis: Boolean = true, // S-Expression
    useBracket: Boolean = true, // script
    isLocation: Boolean = true
  ) extends ParseConfig {
    def withoutLocation = copy(isLocation = false)
  }
  object Config {
    val default = Config()
    val raw = Config(false, false, false, false, false, false)
    val lisp = default.copy(useSingleQuote = false)
  }

  trait LogicalLinesParseState extends ParseReaderWriterState[Config, LogicalLines] {
    def location: Option[ParseLocation]

    def apply(config: Config, evt: ParseEvent): Transition = {
//      println(s"in($this): $evt")
      val r = handle_event(config, evt)
//      println(s"out($this): $r")
      r
    }

    def addChild(config: Config, ps: Vector[Char]): LogicalLinesParseState = RAISE.noReachDefect(s"addChild(${getClass.getSimpleName})")
    def addChildEndTransition(config: Config, ps: Vector[Char]): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")
    def addChildEndTransition(
      config: Config,
      msgs: ParseMessageSequence,
      ps: Vector[Char]
    ): Transition = addChildEndTransition(config, ps)

    protected final def handle_event(config: Config, evt: ParseEvent): Transition =
      evt match {
        case StartEvent => handle_start(config)
        case EndEvent => handle_end(config)
        case m: LineEndEvent => handle_line_end(config, m)
        case m: CharEvent => m.c match {
          case '"' if config.useDoubleQuote => handle_double_quote(config, m)
          case '\'' if config.useSingleQuote => handle_single_quote(config, m)
          case '<' if config.useAngleBracket => handle_open_angle_bracket(config, m)
          case '>' if config.useAngleBracket => handle_close_angle_bracket(config, m)
          case '{' if config.useBrace => handle_open_brace(config, m)
          case '}' if config.useBrace => handle_close_brace(config, m)
          case '(' if config.useParenthesis => handle_open_parenthesis(config, m)
          case ')' if config.useParenthesis => handle_close_parenthesis(config, m)
          case '[' if config.useBracket => handle_open_bracket(config, m)
          case ']' if config.useBracket => handle_close_bracket(config, m)
          case '\n' => handle_newline(config, m)
          case '\r' => handle_carrige_return(config, m)
          case c => handle_character(config, m)
        }
      }

    protected final def handle_start(config: Config): Transition =
      handle_Start(config)

    protected def handle_Start(config: Config): Transition =
      (ParseMessageSequence.empty, start_Result(config), start_State(config))

    protected def start_Result(config: Config): ParseResult[LogicalLines] = 
      ParseSuccess(LogicalLines.empty)

    protected def start_State(config: Config): LogicalLinesParseState = this

    protected final def handle_end(config: Config): Transition =
      handle_End(config)

    protected def handle_End(config: Config): Transition =
      (ParseMessageSequence.empty, end_Result(config), end_State(config))

    protected def end_Result(config: Config): ParseResult[LogicalLines] = 
      RAISE.notImplementedYetDefect(this, "end_Result")

    protected def end_State(config: Config): LogicalLinesParseState = EndState

    protected final def handle_line_end(config: Config, evt: LineEndEvent): Transition =
      handle_Line_End(config, evt)

    protected def handle_Line_End(config: Config, evt: LineEndEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, line_End_State(config, evt))

    protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState = RAISE.notImplementedYetDefect(this, "line_End_State")

    protected final def handle_double_quote(config: Config, evt: CharEvent): Transition =
      handle_Double_Quote(config, evt)

    protected def handle_Double_Quote(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, double_Quote_State(config, evt))

    protected def double_Quote_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      DoubleQuoteState(this, evt.location)

    protected final def handle_single_quote(config: Config, evt: CharEvent): Transition =
      handle_Single_Quote(config, evt)


    protected def handle_Single_Quote(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, single_Quote_State(config, evt))

    protected def single_Quote_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      SingleQuoteState(this, evt.location)

    protected final def handle_open_angle_bracket(config: Config, evt: CharEvent): Transition =
      handle_Open_Angle_Bracket(config, evt)

    protected def handle_Open_Angle_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Angle_Bracket_State(config, evt))

    protected def open_Angle_Bracket_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_close_angle_bracket(config: Config, evt: CharEvent): Transition =
      handle_Close_Angle_Bracket(config, evt)

    protected def handle_Close_Angle_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Angle_Bracket_State(config, evt))

    protected def close_Angle_Bracket_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_open_brace(config: Config, evt: CharEvent): Transition =
      handle_Open_Brace(config, evt)

    protected def handle_Open_Brace(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Brace_State(config, evt))

    protected def open_Brace_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_close_brace(config: Config, evt: CharEvent): Transition =
      handle_Close_Brace(config, evt)

    protected def handle_Close_Brace(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Brace_State(config, evt))

    protected def close_Brace_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_open_parenthesis(config: Config, evt: CharEvent): Transition =
      handle_Open_Parenthesis(config, evt)

    protected def handle_Open_Parenthesis(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Parenthesis_State(config, evt))

    protected def open_Parenthesis_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_close_parenthesis(config: Config, evt: CharEvent): Transition =
      handle_Close_Parenthesis(config, evt)

    protected def handle_Close_Parenthesis(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Parenthesis_State(config, evt))

    protected def close_Parenthesis_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_open_bracket(config: Config, evt: CharEvent): Transition =
      handle_Open_Bracket(config, evt)

    protected def handle_Open_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(config, evt))

    protected def open_Bracket_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_close_bracket(config: Config, evt: CharEvent): Transition =
      handle_Close_Bracket(config, evt)

    protected def handle_Close_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(config, evt))

    protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    protected final def handle_newline(config: Config, evt: CharEvent): Transition =
      handle_Newline(config, evt)

    protected def handle_Newline(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, newline_State(config, evt))

    protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      line_End_State(config, LineEndEvent(evt.location))

    protected final def handle_carrige_return(config: Config, evt: CharEvent): Transition =
      handle_Carrige_Return(config, evt)

    protected def handle_Carrige_Return(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, carrige_Return_State(config, evt))

    protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      evt.next match {
        case Some('\n') => SkipState(
          line_End_State(config, LineEndEvent(evt.location))
        )
        case _ => line_End_State(config, LineEndEvent(evt.location))
      }

    protected final def handle_character(config: Config, evt: CharEvent): Transition =
      handle_Character(config, evt)

    protected def handle_Character(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, character_State(config, evt))

    protected def character_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(evt.c)

    protected def character_State(c: Char): LogicalLinesParseState =
      RAISE.notImplementedYetDefect(this, "character_State")

    protected final def get_location(config: Config): Option[ParseLocation] =
      if (config.isLocation)
        location.flatMap(_.toOption)
      else
        None
  }

  sealed trait AwakeningLogicalLinesParseState extends LogicalLinesParseState {
    override protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    override protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)
  }

  case object InitState extends LogicalLinesParseState {
    val location = None
    override protected def end_Result(config: Config) = ParseSuccess(LogicalLines.empty)
    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState =
      NormalState("", evt.location)
    override protected def character_State(config: Config, evt: CharEvent) = NormalState(evt.c, evt.location)
    override protected def double_Quote_State(config: Config, evt: CharEvent) =
      DoubleQuoteState(NormalState(evt.location), evt.location)
    override protected def single_Quote_State(config: Config, evt: CharEvent) =
      SingleQuoteState(NormalState(evt.location), evt.location)
    override protected def open_Angle_Bracket_State(config: Config, evt: CharEvent) = XmlOpenState(NormalState(evt.location), Some(evt.location))
    // override protected def close_Angle_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect
    override protected def open_Brace_State(config: Config, evt: CharEvent) = JsonState(NormalState(evt.location), evt.location)
    // override protected def close_Brace_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect
    override protected def open_Parenthesis_State(config: Config, evt: CharEvent) = SExpressionState(NormalState(evt.location), evt.location)
    // override protected def close_Parenthesis_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect
    override protected def open_Bracket_State(config: Config, evt: CharEvent) = BracketState(NormalState(evt.location), evt.location)
    // override protected def close_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect
  }

  case object EndState extends LogicalLinesParseState {
    val location = None
    override def apply(config: Config, evt: ParseEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, this)
  }

  case class SkipState(next: Transition) extends LogicalLinesParseState {
    val location = None
    override def apply(config: Config, evt: ParseEvent): Transition = next
  }
  object SkipState {
    def apply(p: LogicalLinesParseState): SkipState = SkipState(
      ParseMessageSequence.empty, ParseResult.empty, p
    )
  }

  case class NormalState(
    cs: Vector[Char],
    location: Option[ParseLocation],
    result: LogicalLines
  ) extends LogicalLinesParseState {
    override def addChild(config: Config, ps: Vector[Char]) = copy(cs = cs ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(cs = cs ++ ps)(config, EndEvent)

    override protected def end_Result(config: Config) = {
      val r = if (cs.isEmpty) result else result :+ LogicalLine(cs.mkString, get_location(config))
      ParseSuccess(r)
    }

    override protected def handle_Newline(config: Config, evt: CharEvent): Transition =
      handle_Line_End(config, LineEndEvent(evt.location))

    override protected def handle_Line_End(config: Config, evt: LineEndEvent): Transition = {
      val r = result :+ LogicalLine(cs.mkString, get_location(config))
      (ParseMessageSequence.empty, ParseSuccess(r), NormalState.empty)
    }

    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState =
      NormalState(Vector.empty, Some(evt.location), result :+ cs.mkString)
    override protected def character_State(config: Config, evt: CharEvent) =
      if (location.isDefined)
        copy(cs = cs :+ evt.c)
      else
        copy(cs = cs :+ evt.c, location = Some(evt.location))
    override protected def double_Quote_State(config: Config, evt: CharEvent) = DoubleQuoteState(this, evt.location)
    override protected def single_Quote_State(config: Config, evt: CharEvent) = SingleQuoteState(this, evt.location)
    override protected def open_Angle_Bracket_State(config: Config, evt: CharEvent) = XmlOpenState(this, Some(evt.location))
    override protected def close_Angle_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Angle_Bracket_State")
    override protected def open_Brace_State(config: Config, evt: CharEvent) = JsonState(this, evt.location)
    override protected def close_Brace_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Brace_State")
    override protected def open_Parenthesis_State(config: Config, evt: CharEvent) = SExpressionState(this, evt.location)
    override protected def close_Parenthesis_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Parenthesis_State")
    override protected def open_Bracket_State(config: Config, evt: CharEvent) = BracketState(this, evt.location)
    override protected def close_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Bracket_State")
  }
  object NormalState {
    val empty = NormalState(Vector.empty, None, LogicalLines.empty)
    def apply(p: Char, location: ParseLocation): NormalState = NormalState(Vector(p), Some(location), LogicalLines.empty)
    def apply(p: String, location: ParseLocation): NormalState = NormalState(Vector.empty, Some(location), LogicalLines(p))
    def apply(location: ParseLocation): NormalState = NormalState(Vector.empty, Some(location), LogicalLines.empty)
  }

  case class SExpressionState(
    parent: LogicalLinesParseState,
    count: Int,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AwakeningLogicalLinesParseState {
    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)

    override protected def open_Parenthesis_State(config: Config, evt: CharEvent) =
      copy(count = count + 1, text = text :+ evt.c)
    override protected def close_Parenthesis_State(config: Config, evt: CharEvent) = {
      count - 1 match {
        case 0 => parent.addChild(config, text :+ evt.c)
        case n => copy(count = n, text = text :+ evt.c)
      }
    }
    override protected def character_State(c: Char): LogicalLinesParseState =
      copy(text = text :+ c)
  }
  object SExpressionState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): SExpressionState = SExpressionState(parent, 1, Vector('('), Some(location))
  }

  case class JsonState(
    parent: LogicalLinesParseState,
    count: Int,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AwakeningLogicalLinesParseState {
    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)

    override protected def open_Brace_State(config: Config, evt: CharEvent) =
      copy(count = count + 1, text = text :+ evt.c)
    override protected def close_Brace_State(config: Config, evt: CharEvent) = {
      count - 1 match {
        case 0 => parent.addChild(config, text :+ evt.c)
        case n => copy(count = n, text = text :+ evt.c)
      }
    }
    override protected def character_State(c: Char): LogicalLinesParseState =
      copy(text = text :+ c)
  }
  object JsonState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): JsonState = JsonState(parent, 1, Vector('{'), Some(location))
  }

  case class XmlOpenState(
    parent: LogicalLinesParseState,
    location: Option[ParseLocation],
    tag: Vector[Char] = Vector.empty
  ) extends AwakeningLogicalLinesParseState {
    lazy val tagName = tag.takeWhile(_not_delimiterp).mkString
    lazy val tagString = s"<${tag.mkString}>"

    private def _not_delimiterp(p: Char) = !_delimiterp(p)

    private def _delimiterp(p: Char) = p match {
      case ' ' => true
      case _ => false
    }

    override def addChild(config: Config, ps: Vector[Char]) = copy(tag = tag ++ ps)

    override protected def handle_End(config: Config): Transition = RAISE.notImplementedYetDefect(this, "handle_End") // TODO error

    override protected def character_State(config: Config, evt: CharEvent) = {
      evt.c match {
        case '/' => evt.next match {
          case Some('>') => SkipState(
            parent.addChild(config, ('<' +: tag) ++ Vector('/', '>'))
          )
          case _ => RAISE.notImplementedYetDefect(this, "character_State") // TODO error
        }
        case c => copy(tag = tag :+ c)
      }
    }

    override protected def close_Angle_Bracket_State(config: Config, evt: CharEvent) = {
      XmlTextState(this, evt.location)
    }
  }
  object XmlOpenState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): XmlOpenState =
      XmlOpenState(parent, Some(location))
  }

  case class XmlCloseState(
    text: XmlTextState,
    location: Option[ParseLocation],
    tag: Vector[Char] = Vector.empty
  ) extends AwakeningLogicalLinesParseState {
    lazy val tagName = tag.mkString
    lazy val tagString = s"</${tagName}>"

    override protected def close_Angle_Bracket_State(config: Config, evt: CharEvent) = {
      // println(s"XmlTextState#close_Angle_Bracket_State: $evt")
      if (tagName == text.open.tagName)
        text.open.parent.addChild(config, s"""${text.open.tagString}${text.textString}${tagString}""".toVector)
      else
        RAISE.notImplementedYetDefect(this, "close_Angle_Bracket_State") // TODO error
    }
    override protected def character_State(c: Char): LogicalLinesParseState =
      if (c == '/')
        this
      else
        copy(tag = tag :+ c)
  }
  object XmlCloseState {
    def apply(parent: XmlTextState, location: ParseLocation): XmlCloseState =
      XmlCloseState(parent, Some(location))
  }

  case class XmlTextState(
    open: XmlOpenState,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AwakeningLogicalLinesParseState {
    lazy val textString = text.mkString

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      RAISE.notImplementedYetDefect(this, textString)

    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)

    override protected def open_Angle_Bracket_State(config: Config, evt: CharEvent) = {
      // println(s"XmlTextState#close_Angle_Bracket_State: $evt")
      if (evt.next == Some('/'))
        XmlCloseState(this, evt.location)
      else
        XmlOpenState(this, evt.location)
    }
    override protected def character_State(c: Char): LogicalLinesParseState =
      copy(text = text :+ c)
  }
  object XmlTextState {
    def apply(open: XmlOpenState, location: ParseLocation): XmlTextState =
      XmlTextState(open, Vector.empty, Some(location))
  }

  case class BracketState(
    parent: LogicalLinesParseState,
    location: Option[ParseLocation],
    text: Vector[Char] = Vector.empty
  ) extends AwakeningLogicalLinesParseState {
    override protected def handle_End(config: Config): Transition = RAISE.notImplementedYetDefect(this, "handle_End") // TODO error
    override protected def character_State(c: Char) = copy(text = text :+ c)
    override protected def close_Bracket_State(config: Config, evt: CharEvent) =
      parent.addChild(config, '[' +: text :+ ']')
  }
  object BracketState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): BracketState =
      BracketState(parent, Some(location))
  }

  case class DoubleQuoteState(
    parent: LogicalLinesParseState,
    location: Option[ParseLocation],
    text: Vector[Char] = Vector.empty
  ) extends AwakeningLogicalLinesParseState {
    override protected def handle_End(config: Config): Transition =
      parent.addChildEndTransition(
        config,
        ParseMessageSequence.warning("In '\"', reach to unexpected end."),
        '"' +: text :+ '"'
      )
    override protected def character_State(c: Char) = copy(text = text :+ c)
    override protected def double_Quote_State(config: Config, evt: CharEvent) =
      parent.addChild(config, '"' +: text :+ '"')
  }
  object DoubleQuoteState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): DoubleQuoteState =
      DoubleQuoteState(parent, Some(location))
  }

  case class SingleQuoteState(
    parent: LogicalLinesParseState,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AwakeningLogicalLinesParseState {
    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      ParseResult.error("Unpredictable end in a single quote string.", "シングルクオートの文字列中で最後になりました。", location)

    override protected def character_State(c: Char) = copy(text = text :+ c)

    override protected def single_Quote_State(config: Config, evt: CharEvent) =
      parent.addChild(config, '"' +: text :+ '"')
  }
  object SingleQuoteState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): SingleQuoteState =
      SingleQuoteState(parent, Vector.empty, Some(location))
  }

  // case class RawStringState() extends AwakeningLogicalLinesParseState {
  // }
}
