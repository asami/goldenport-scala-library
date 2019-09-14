package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 28, 2018
 *  version Sep. 30, 2018
 *  version Oct. 26, 2018
 *  version Jan.  3, 2019
 *  version Feb. 16, 2019
 *  version Mar. 10, 2019
 *  version May.  6, 2019
 *  version Jun. 30, 2019
 *  version Jul. 17, 2019
 * @version Sep. 12, 2019
 * @author  ASAMI, Tomoharu
 */
case class LogicalTokens(
  tokens: Vector[LogicalToken]
) {
  def isEmpty = tokens.isEmpty
  lazy val head = tokens.head
  lazy val tail = LogicalTokens(tokens.tail)
  def +(p: LogicalTokens) = LogicalTokens(tokens ++ p.tokens)
  def +(p: LogicalToken) = LogicalTokens(tokens :+ p)
  def :+(p: LogicalToken) = LogicalTokens(tokens :+ p)
  def +:(p: LogicalToken) = LogicalTokens(p +: tokens)

  lazy val tokensWithEnd = tokens :+ EndToken
}

object LogicalTokens {
  type Transition = (ParseMessageSequence, ParseResult[LogicalTokens], LogicalTokensParseState)

  val empty = LogicalTokens(Vector.empty)

  implicit object LogicalTokensMonoid extends Monoid[LogicalTokens] {
    def zero = LogicalTokens.empty
    def append(lhs: LogicalTokens, rhs: => LogicalTokens) = lhs + rhs
  }

  def apply(p: LogicalToken, ps: LogicalToken*): LogicalTokens =
    LogicalTokens(p +: ps.toVector)

  def parse(p: String): LogicalTokens = parse(Config.default, p)

  def parse(config: Config, p: String): LogicalTokens = {
    val parser = ParseReaderWriterStateClass[Config, LogicalTokens](config, NormalState.init)
    val (messages, result, state) = parser.apply(p)
    result match {
      case ParseSuccess(tokens, _) => tokens.concatenate
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseSexpr(p: String): LogicalTokens = parse(Config.sexpr, p)

  def parseDebug(p: String): LogicalTokens = parse(Config.debug, p)

  sealed trait Tokenizer {
  }

  trait SimpleTokenizer extends Tokenizer {
    def accept(config: Config, p: String, location: ParseLocation): Option[LogicalTokens] = accept_Token(config, p, location).map(LogicalTokens(_))
    protected def accept_Token(config: Config, p: String, location: ParseLocation): Option[LogicalToken] = None
  }

  trait ComplexTokenizer extends Tokenizer {
    def accept(config: Config, parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState]
  }

  case class Config(
    spaces: Vector[Char],
    delimiters: Vector[Char],
    comments: Vector[Char],
    simpleTokenizers: Vector[SimpleTokenizer],
    complexTokenizers: Vector[ComplexTokenizer],
    useSingleQuoteToken: Boolean,
    isDebug: Boolean
  ) extends ParseConfig {
    private val _simple_tokenizers = simpleTokenizers.toStream
    private val _complex_tokenizers = complexTokenizers.toStream

    def isSpace(c: Char): Boolean = spaces.contains(c)

    def isDelimiter(c: Char): Boolean = delimiters.contains(c)

    def isComment(evt: CharEvent): Boolean = comments.contains(evt.c)

    def handle(parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] =
      _complex_tokenizers.flatMap(_.accept(this, parent, evt)).headOption

    def getTokens(config: Config, p: String, l: ParseLocation): Option[LogicalTokens] = _simple_tokenizers.flatMap(_.accept(config, p, l)).headOption
  }
  object Config {
    val default = Config(
      " \t\n\r\f".toVector,
      "(){}[]".toVector, // "(){}[],".toVector, get rid of ',' for expression token
      ";".toVector,
      Vector(
        NumberToken,
        LocalDateToken, LocalDateTimeToken, LocalTimeToken, DateTimeToken, MonthDayToken,
        DateTimeIntervalToken,
        IntervalToken, RangeToken,
        LxsvToken, XsvToken,
        UrlToken, UrnToken,
        PathToken, ExpressionToken
      ),
      Vector(JsonParser(), XmlParser()),
      false,
      false
    )
    val sexpr = default.copy(useSingleQuoteToken = true)
    val debug = default.copy(isDebug = true)
    val xsv = default.copy(
      "".toVector,
      " \t,;".toVector
    )
    val csv = default.copy(
      "".toVector,
      ",".toVector
    )
    val tsv = default.copy(
      "".toVector,
      "\t".toVector
    )
    val scsv = default.copy(
      "".toVector,
      ";".toVector
    )
    val ssv = default.copy(
      "".toVector,
      " \t".toVector
    )
  }

  trait LogicalTokensParseState extends ParseReaderWriterState[Config, LogicalTokens] {
    protected def use_Tokenizer = true
    protected def use_Delimiter = true
    protected def use_Double_Quote = true
    protected def use_Single_Quote = true
    protected def use_Bracket = true
    protected def use_Dollar = true

    def apply(config: Config, evt: ParseEvent): Transition = {
      if (config.isDebug)
        println(s"IN ($this): $evt")
      val r = handle_event(config, evt)
      if (config.isDebug)
        println(s"OUT ($this): $r")
      r
    }

    def addChildState(config: Config, p: Vector[Char]): LogicalTokensParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildState(config: Config, p: LogicalToken): LogicalTokensParseState =
      addChildState(config, LogicalTokens(p))

    def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildTransition(config: Config, p: LogicalTokens): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildTransition(config: Config, p: LogicalToken, evt: CharEvent): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildEndTransition(config: Config, p: LogicalToken): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")

    protected final def handle_event(config: Config, evt: ParseEvent): Transition =
      evt match {
        case StartEvent => handle_start(config)
        case EndEvent => handle_end(config)
        case m: LineEndEvent => handle_line_end(config, m)
        case m: CharEvent => handle_tokenizer(config, m) getOrElse {
          m.c match {
            case '"' if use_Double_Quote => handle_double_quote(config, m)
            case '\'' if use_Single_Quote => handle_single_quote(config, m)
            case '[' if use_Bracket => handle_open_bracket(config, m)
            case ']' if use_Bracket => handle_close_bracket(config, m)
            case '$' if use_Dollar => handle_dollar(config, m)
            case c if config.isSpace(c) => handle_space(config, m)
            case c if use_Delimiter && config.isDelimiter(c) => handle_delimiter(config, m)
            case c if config.isComment(m) => handle_comment(config, m) // TODO handle not " ;" (e.g. "id:1;name:taro")
            case _ => handle_character(config, m)
          }
        }
        // case m: CharEvent => m.c match {
        //   case '<' => handle_open_angle_bracket(config, m)
        //   case '>' => handle_close_angle_bracket(config, m)
        //   case '{' => handle_open_brace(config, m)
        //   case '}' => handle_close_brace(config, m)
        //   case '(' => handle_open_parenthesis(config, m)
        //   case ')' => handle_close_parenthesis(config, m)
        //   case '[' => handle_open_bracket(config, m)
        //   case ']' => handle_close_bracket(config, m)
        //   case '\n' => handle_newline(config, m)
        //   case '\r' => handle_carrige_return(config, m)
        //   case c => handle_character(config, m)
        // }
      }

    protected final def handle_tokenizer(config: Config, evt: CharEvent): Option[Transition] =
      if (use_Tokenizer)
        config.handle(this, evt).map(x =>
          // (ParseMessageSequence.empty, end_Result(config), x)
          (ParseMessageSequence.empty, ParseResult.empty, x)
        )
      else
        None

    protected final def handle_start(config: Config): Transition =
      handle_Start(config)

    protected def handle_Start(config: Config): Transition =
      (ParseMessageSequence.empty, start_Result(config), start_State(config))

    protected def start_Result(config: Config): ParseResult[LogicalTokens] = 
      ParseSuccess(LogicalTokens.empty)

    protected def start_State(config: Config): LogicalTokensParseState = this

    protected final def handle_end(config: Config): Transition =
      handle_End(config)

    protected def handle_End(config: Config): Transition =
      (ParseMessageSequence.empty, end_Result(config), end_State(config))

    protected def end_Result(config: Config): ParseResult[LogicalTokens] =
      ParseSuccess(LogicalTokens.empty)

    protected def end_State(config: Config): LogicalTokensParseState = EndState

    protected final def handle_line_end(config: Config, evt: LineEndEvent): Transition =
      handle_Line_End(config, evt)

    protected def handle_Line_End(config: Config, evt: LineEndEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, line_End_State(config, evt))

    protected def line_End_State(config: Config, evt: LineEndEvent): LogicalTokensParseState = RAISE.notImplementedYetDefect(s"line_End_State(${getClass.getSimpleName})")

    protected def handle_space(config: Config, evt: CharEvent): Transition =
      handle_Space(config, evt)

    protected def handle_Space(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, space_State(config, evt))

    protected def space_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected def handle_delimiter(config: Config, evt: CharEvent): Transition =
      handle_Delimiter(config, evt)

    protected def handle_Delimiter(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, delimiter_State(config, evt))

    protected def delimiter_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected def handle_comment(config: Config, evt: CharEvent): Transition =
      handle_Comment(config, evt)

    protected def handle_Comment(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, comment_State(config, evt))

    protected def comment_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      EndState

    protected final def handle_double_quote(config: Config, evt: CharEvent): Transition =
      handle_Double_Quote(config, evt)

    protected def handle_Double_Quote(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, double_Quote_State(config, evt))

    protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected final def handle_single_quote(config: Config, evt: CharEvent): Transition =
      handle_Single_Quote(config, evt)

    protected def handle_Single_Quote(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, single_Quote_State(config, evt))

    protected def single_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected final def handle_open_bracket(config: Config, evt: CharEvent): Transition =
      handle_Open_Bracket(config, evt)

    protected def handle_Open_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(config, evt))

    protected def open_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected final def handle_close_bracket(config: Config, evt: CharEvent): Transition =
      handle_Close_Bracket(config, evt)

    protected def handle_Close_Bracket(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(config, evt))

    protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected final def handle_dollar(config: Config, evt: CharEvent): Transition =
      handle_Dollar(config, evt)

    protected def handle_Dollar(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, dollar_State(config, evt))

    protected def dollar_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(config, evt)

    protected final def handle_character(config: Config, evt: CharEvent): Transition =
      handle_Character(config, evt)

    protected def handle_Character(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, character_State(config, evt))

    protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(evt.c)

    protected def character_State(c: Char): LogicalTokensParseState =
      RAISE.notImplementedYetDefect(s"character_State(${getClass.getSimpleName}): $c[${c.toInt}]")
    //
    protected final def to_tokens(config: Config, p: Vector[Char], l: ParseLocation): LogicalTokens = to_tokens(config, p.mkString, l)

    protected final def to_tokens(config: Config, p: String, l: ParseLocation): LogicalTokens =
      config.getTokens(config, p, l) getOrElse LogicalTokens(AtomToken(p, l))

    // XXX
    // protected final def handle_open_angle_bracket(config: Config, evt: CharEvent): Transition =
    //   handle_Open_Angle_Bracket(config, evt)

    // protected def handle_Open_Angle_Bracket(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, open_Angle_Bracket_State(config, evt))

    // protected def open_Angle_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_close_angle_bracket(config: Config, evt: CharEvent): Transition =
    //   handle_Close_Angle_Bracket(config, evt)

    // protected def handle_Close_Angle_Bracket(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, close_Angle_Bracket_State(config, evt))

    // protected def close_Angle_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_open_brace(config: Config, evt: CharEvent): Transition =
    //   handle_Open_Brace(config, evt)

    // protected def handle_Open_Brace(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, open_Brace_State(config, evt))

    // protected def open_Brace_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_close_brace(config: Config, evt: CharEvent): Transition =
    //   handle_Close_Brace(config, evt)

    // protected def handle_Close_Brace(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, close_Brace_State(config, evt))

    // protected def close_Brace_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_open_parenthesis(config: Config, evt: CharEvent): Transition =
    //   handle_Open_Parenthesis(config, evt)

    // protected def handle_Open_Parenthesis(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, open_Parenthesis_State(config, evt))

    // protected def open_Parenthesis_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_close_parenthesis(config: Config, evt: CharEvent): Transition =
    //   handle_Close_Parenthesis(config, evt)

    // protected def handle_Close_Parenthesis(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, close_Parenthesis_State(config, evt))

    // protected def close_Parenthesis_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_open_bracket(config: Config, evt: CharEvent): Transition =
    //   handle_Open_Bracket(config, evt)

    // protected def handle_Open_Bracket(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(config, evt))

    // protected def open_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_close_bracket(config: Config, evt: CharEvent): Transition =
    //   handle_Close_Bracket(config, evt)

    // protected def handle_Close_Bracket(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(config, evt))

    // protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   character_State(config, evt)

    // protected final def handle_newline(config: Config, evt: CharEvent): Transition =
    //   handle_Newline(config, evt)

    // protected def handle_Newline(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, newline_State(config, evt))

    // protected def newline_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   line_End_State(config, LineEndEvent())

    // protected final def handle_carrige_return(config: Config, evt: CharEvent): Transition =
    //   handle_Carrige_Return(config, evt)

    // protected def handle_Carrige_Return(config: Config, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, carrige_Return_State(config, evt))

    // protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalTokensParseState =
    //   evt.next match {
    //     case Some('\n') => SkipState(
    //       line_End_State(config, LineEndEvent())
    //     )
    //     case _ => line_End_State(config, LineEndEvent())
    //   }
  }

  trait RawLogicalTokensParseState extends LogicalTokensParseState {
    override val use_Tokenizer = false
    override val use_Delimiter = false
    override val use_Double_Quote = false
    override val use_Single_Quote = false
    override val use_Bracket = false
  }

  trait ChildLogicalTokensParseState extends LogicalTokensParseState {
    def parent: LogicalTokensParseState

    protected final def flush_state(config: Config, p: LogicalTokens): LogicalTokensParseState =
      flush_State(config, p)

    protected final def flush_state(config: Config, p: LogicalToken): LogicalTokensParseState =
      flush_State(config, p)

    protected final def flush_state(config: Config): LogicalTokensParseState =
      flush_State(config)

    protected final def flush_tokens(config: Config): LogicalTokens =
      flush_Tokens(config)

    protected def flush_State(config: Config, p: LogicalTokens): LogicalTokensParseState =
      parent.addChildState(config, flush_tokens(config))

    protected def flush_State(config: Config, p: LogicalToken): LogicalTokensParseState =
      flush_State(config, LogicalTokens(p))

    protected def flush_State(config: Config): LogicalTokensParseState =
      flush_State(config, LogicalTokens.empty)

    protected def flush_Tokens(config: Config): LogicalTokens

    override def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState =
      flush_state(config, p)

    override def addChildTransition(config: Config, p: LogicalToken, evt: CharEvent): Transition =
      flush_state(config, p).apply(config, evt)

    override def addChildEndTransition(config: Config, p: LogicalToken): Transition =
      flush_state(config, p).apply(config, EndEvent)

    override protected def end_Result(config: Config): ParseResult[LogicalTokens] =
      ParseSuccess(flush_tokens(config))
  }

  trait WithTerminalLogicalTokensParseState extends LogicalTokensParseState {
    override val use_Tokenizer = false
    override val use_Delimiter = false
    override val use_Double_Quote = false
    override val use_Single_Quote = false
    override val use_Bracket = false
  }

  trait WithoutTerminalLogicalTokensParseState extends ChildLogicalTokensParseState {
    def parent: LogicalTokensParseState

    override val use_Tokenizer = false
    override val use_Double_Quote = false
    override val use_Single_Quote = false
    override val use_Bracket = false

    override protected def space_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      flush_state(config)

    override protected def delimiter_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      flush_state(config, DelimiterToken(evt.c, evt.location))
  }

  trait StringLiteralLogicalTokensParseState extends LogicalTokensParseState {
    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.next == Some('"') && evt.next2 == Some('"'))
        SkipState(SkipState(RawStringState(this, evt)))
      else
        DoubleQuoteState(this, evt)

    override protected def single_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      SingleQuoteState(this, evt)
  }

  case object EndState extends LogicalTokensParseState {
    override def apply(config: Config, evt: ParseEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, this)
  }

  case class SkipState(next: Transition) extends LogicalTokensParseState {
    override def apply(config: Config, evt: ParseEvent): Transition = next
  }
  object SkipState {
    def apply(p: LogicalTokensParseState): SkipState = SkipState(
      ParseMessageSequence.empty, ParseResult.empty, p
    )
  }

  case class SpaceState(
    parent: LogicalTokensParseState,
    spaces: Vector[Char],
    location: Option[ParseLocation]
  ) extends RawLogicalTokensParseState {
    override def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState =
      parent.addChildState(config, SpaceToken(spaces.mkString, None) +: p)

    override protected def handle_End(config: Config): Transition =
      parent.addChildEndTransition(config, SpaceToken(spaces.mkString, location))

    override protected def handle_Double_Quote(config: Config, evt: CharEvent): Transition =
      handle_character(config, evt)

    override protected def handle_Single_Quote(config: Config, evt: CharEvent): Transition =
      handle_character(config, evt)

    override protected def handle_Character(config: Config, evt: CharEvent): Transition =
      if (config.isSpace(evt.c))
        (ParseMessageSequence.empty, ParseResult.empty, copy(spaces = spaces :+ evt.c))
      else
        parent.addChildTransition(config, SpaceToken(spaces.mkString, location), evt)

    override protected def space_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      this
  }

  object SpaceState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): SpaceState =
      SpaceState(p, Vector(evt.c), Some(evt.location))
  }

  case class CommentState(
    parent: LogicalTokensParseState
  ) extends LogicalTokensParseState {
    // TODO end mark
  }

  case class DoubleQuoteState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def use_Tokenizer = false
    override protected def use_Delimiter = false
    override protected def use_Single_Quote = false
    override protected def use_Bracket = false
    override protected def use_Dollar = false

    override def addChildState(config: Config, p: Vector[Char]): LogicalTokensParseState =
      copy(text = text ++ p)

    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      // println(s"DoubleQuote#double_Quote_State: $evt")
      parent.addChildState(config, DoubleStringToken(text.mkString, location, prefix))
    }

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      // println(s"DoubleQuoteState#double_Quote_State: $evt")
      if (evt.c == '\\')
        StringQuoteEscapeState(this)
      else
        copy(text = text :+ evt.c)
    }
  }
  object DoubleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DoubleQuoteState =
      DoubleQuoteState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DoubleQuoteState =
      DoubleQuoteState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class StringQuoteEscapeState(
    parent: LogicalTokensParseState
  ) extends LogicalTokensParseState {
    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      evt.c match {
        case '\\' => parent.addChildState(config, Vector('\\'))
        case '"' => parent.addChildState(config, Vector('"'))
        case '\'' => parent.addChildState(config, Vector('\''))
        case 't' => parent.addChildState(config, Vector('\t'))
        case 'f' => parent.addChildState(config, Vector('\f'))
        case 'n' => parent.addChildState(config, Vector('\n'))
        case 'r' => parent.addChildState(config, Vector('\r'))
        case c => parent.addChildState(config, Vector(c))
      }
  }

  case class SingleQuoteState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def use_Tokenizer = false
    override protected def use_Delimiter = false
    override protected def use_Double_Quote = false
    override protected def use_Bracket = false
    override protected def use_Dollar = false

    override def addChildState(config: Config, p: Vector[Char]): LogicalTokensParseState =
      copy(text = text ++ p)

    override protected def single_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(config, SingleStringToken(text.mkString, location))

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '\\')
        StringQuoteEscapeState(this)
      else
        copy(text = text :+ evt.c)
  }
  object SingleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): SingleQuoteState =
      SingleQuoteState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: String): SingleQuoteState =
      SingleQuoteState(p, Vector.empty, Some(evt.location), Some(prefix))
  }

  case class RawStringState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    inClosing: Boolean = false
  ) extends LogicalTokensParseState {
    override protected def use_Tokenizer = false
    override protected def use_Delimiter = false
    override protected def use_Single_Quote = false
    override protected def use_Bracket = false
    override protected def use_Dollar = false

    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalTokensParseState =
      parent.addChildState(config, RawStringToken(text.mkString, location, prefix))

    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      // println(s"RawStringState#double_Quote_State: $evt")
      if (evt.c == '"' && evt.next == Some('"') && evt.next2 == Some('"')) {
        if (inClosing)
          copy(text = text :+ evt.c)
        else
          copy(inClosing = true)
      } else if (evt.c == '"' && evt.next == Some('"')) {
        if (inClosing)
          SkipState(parent.addChildState(config, RawStringToken(text.mkString, location, prefix)))
        else
          copy(text = text :+ evt.c)
      } else {
        copy(text = text :+ evt.c)
      }
    }

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      // println(s"RawStringState#character_State: $evt")
      copy(text = text :+ evt.c)
    }
  }
  object RawStringState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): RawStringState =
      RawStringState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): RawStringState =
      RawStringState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class BracketState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    private def _token = prefix.map(x =>
      if (x.contains('/'))
        _path_token(x)
      else
        _bracket_token
    ).getOrElse(_bracket_token)

    private def _bracket_token = BracketToken(text.mkString, location, prefix)
    private def _path_token(s: String) = PathToken(s"${s}[${text.mkString}]", location)

    override protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(config, _token)

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object BracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): BracketState =
      BracketState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): BracketState =
      BracketState(p, Vector.empty, Some(evt.location.adjustPrefix(prefix)), prefix)
  }

  case class DoubleBracketState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("]]"))
        SkipState(parent.addChildState(config, DoubleBracketToken(text.mkString, location, prefix)))
      else
        copy(text = text :+ evt.c)

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object DoubleBracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DoubleBracketState =
      DoubleBracketState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DoubleBracketState =
      DoubleBracketState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class RawBracketState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def close_Bracket_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("]]]"))
        SkipState(SkipState(parent.addChildState(config, RawBracketToken(text.mkString, location, prefix))))
      else
        copy(text = text :+ evt.c)

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object RawBracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): RawBracketState =
      RawBracketState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): RawBracketState =
      RawBracketState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class DollarState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends RawLogicalTokensParseState {
    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = 
      if (evt.c == '{')
        ScriptState(parent, Vector.empty, location, prefix)
      else
        ShortScriptState(parent, Vector(evt.c), location, prefix)
  }
  object DollarState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DollarState =
      DollarState(p, Vector.empty, Some(evt.location), None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DollarState =
      DollarState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class ShortScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends WithoutTerminalLogicalTokensParseState {
    // protected def flush_State(config: Config, p: LogicalTokens): LogicalTokensParseState = {
    //   parent.addChildState(config, flush_tokens(config))
    // }

    protected def flush_Tokens(config: Config): LogicalTokens =
      LogicalTokens(ScriptToken(text.mkString, location, prefix))

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object ShortScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ShortScriptState =
    //   ShortScriptState(p, Vector.empty, Some(evt.location), None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ShortScriptState =
    //   ShortScriptState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class ScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends WithTerminalLogicalTokensParseState {
    // protected def flush_State(config: Config, p: LogicalTokens): LogicalTokensParseState = {
    //   parent.addChildState(config, flush_tokens(config))
    // }

    // protected def flush_Tokens(config: Config): LogicalTokens =
    //   LogicalTokens(ScriptToken(text.mkString, location))

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = 
      if (evt.isMatch("{{"))
        SkipState(ExtraScriptState(parent, Vector.empty, location, prefix))
      else
        LongScriptState(parent, evt.c, location, prefix)
  }
  object ScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ScriptState =
    //   ScriptState(p, Vector.empty, Some(evt.location), None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ScriptState =
    //   ScriptState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class LongScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    count: Int = 1
  ) extends WithTerminalLogicalTokensParseState {
    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState = {
      val c = evt.c
      // println(s"LongScriptState $c, $count")
      c match {
        case '{' => copy(count = count + 1, text = text :+ c)
        case '}' => 
          count - 1 match {
            case 0 => parent.addChildState(config, ScriptToken(text.mkString, location, prefix))
            case n => copy(count = n, text = text :+ c)
          }
        case _ => copy(text = text :+ c)
      }
    }
  }
  object LongScriptState {
    def apply(p: LogicalTokensParseState, c: Char, location: Option[ParseLocation], prefix: Option[String]): LongScriptState = {
      val n = c match {
        case '{' => 2
        case _ => 1
      }
      LongScriptState(p, Vector(c), location, prefix, n)
    }
  }

  case class ExtraScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends WithTerminalLogicalTokensParseState {
    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("}}}"))
        SkipState(SkipState(parent.addChildState(config, ScriptToken(text.mkString, location, prefix))))
      else
        copy(text = text :+ evt.c)
  }
  object ExtraScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ExtraScriptState =
    //   ExtraScriptState(p, Vector.empty, Some(evt.location), None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ExtraScriptState =
    //   ExtraScriptState(p, Vector.empty, Some(evt.location), prefix)
  }

  case class NormalState(
    cs: Vector[Char],
    tokens: LogicalTokens,
    location: ParseLocation
  ) extends StringLiteralLogicalTokensParseState {
    private def _flush(config: Config): NormalState = {
      if (cs.isEmpty)
        this
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(config, cs.mkString, location))
    }

    private def _flush(config: Config, t: LogicalToken): NormalState = {
      if (cs.isEmpty)
        copy(tokens = tokens + t)
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(config, cs.mkString, location) + t)
    }

    private def _flush(config: Config, t: LogicalTokens): NormalState = {
      if (cs.isEmpty)
        copy(tokens = tokens + t)
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(config, cs.mkString, location) + t)
    }

    override def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState =
      _flush(config, p)

    override def addChildTransition(config: Config, p: LogicalToken, evt: CharEvent): Transition =
      _flush(config, p).apply(config, evt)

    override def addChildEndTransition(config: Config, p: LogicalToken): Transition =
      _flush(config, p).apply(config, EndEvent)

    override protected def end_Result(config: Config): ParseResult[LogicalTokens] =
      ParseSuccess(_flush(config).tokens)

    override protected def space_State(config: Config, evt: CharEvent) =
      SpaceState(this, evt)

    override protected def delimiter_State(config: Config, evt: CharEvent) = {
      val a = if (cs.isEmpty) {
        LogicalTokens(DelimiterToken(evt.c, evt.location))
      } else {
        val b = to_tokens(config, cs.mkString, location)
        b + DelimiterToken(evt.c, evt.location)
      }
      val l = evt.location
      copy(Vector.empty, tokens + a, l)
    }

    override protected def double_Quote_State(config: Config, evt: CharEvent) = {
      // println(s"NormalState#double_Quote_State: $evt")
      // println(s"""NormalState#double_Quote_State - ${evt.isMatch("\"\"\"")}""")
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      if (evt.isMatch("\"\"\""))
        SkipState(SkipState(RawStringState(newparent, evt, prefix)))
      else
        DoubleQuoteState(newparent, evt, prefix)
    }

    override protected def single_Quote_State(config: Config, evt: CharEvent) =
      if (config.useSingleQuoteToken) {
        _flush(config, SingleQuoteToken(Some(evt.location)))
      } else {
        val prefix = cs.mkString
        val newparent = copy(cs = Vector.empty)
        SingleQuoteState(newparent, evt, prefix)
      }

    override protected def open_Bracket_State(config: Config, evt: CharEvent) = {
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      if (evt.isMatch("[[["))
        SkipState(SkipState(RawBracketState(newparent, evt, prefix)))
      else if (evt.isMatch("[["))
        SkipState(DoubleBracketState(newparent, evt, prefix))
      else
        BracketState(newparent, evt, prefix)
    }

    override protected def dollar_State(config: Config, evt: CharEvent) = {
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      DollarState(newparent, evt, prefix)
    }

    override protected def character_State(config: Config, evt: CharEvent) =
      if (cs.isEmpty)
        copy(cs = cs :+ evt.c, location = evt.location)
      else
        copy(cs = cs :+ evt.c)
  }
  object NormalState {
    val init = NormalState(Vector.empty, LogicalTokens.empty, ParseLocation.start)
  }
}
