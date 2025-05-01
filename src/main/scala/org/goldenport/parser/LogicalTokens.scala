package org.goldenport.parser

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.context.DateTimeContext

/*
 * @since   Aug. 28, 2018
 *  version Sep. 30, 2018
 *  version Oct. 30, 2018
 *  version Jan.  3, 2019
 *  version Feb. 16, 2019
 *  version Mar. 10, 2019
 *  version May.  6, 2019
 *  version Jun. 30, 2019
 *  version Jul. 17, 2019
 *  version Sep. 28, 2019
 *  version Oct. 28, 2019
 *  version Nov. 10, 2019
 *  version Jan. 30, 2020
 *  version Feb. 29, 2020
 *  version Mar.  1, 2020
 *  version Jan. 30, 2021
 *  version Apr. 21, 2021
 *  version Jun. 17, 2021
 *  version Sep.  7, 2024
 * @version Oct. 22, 2024
 * @author  ASAMI, Tomoharu
 */
case class LogicalTokens(
  tokens: Vector[LogicalToken]
) {
  def raw: String = tokens.map(_.raw).mkString
  def isEmpty = tokens.isEmpty
  lazy val head = try {
    tokens.head
  } catch {
    case NonFatal(e) => RAISE.illegalStateFault("LogicalTokens#head empty")
  }
  lazy val tail = LogicalTokens(tokens.tail)
  def +(p: LogicalTokens) = LogicalTokens(tokens ++ p.tokens)
  def +(p: LogicalToken) = LogicalTokens(tokens :+ p)
  def :+(p: LogicalToken) = LogicalTokens(tokens :+ p)
  def +:(p: LogicalToken) = LogicalTokens(p +: tokens)

  lazy val tokensWithEnd = tokens :+ EndToken

  def toStringToken: StringToken = SingleStringToken(tokens.map(_.raw).mkString)

  def makeToken: LogicalToken = tokens.length match {
    case 0 => EmptyToken
    case 1 => head.clearLocation
    case _ => toStringToken.clearLocation
  }
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

  def parse(config: Config, p: String): LogicalTokens = parse(Context.create(config), p)

  def parse(context: Context, p: String): LogicalTokens = {
    val parser = ParseReaderWriterStateClass[Context, LogicalTokens](context, NormalState.init)
    val (messages, result, state) = parser.apply(p)
    result match {
      case ParseSuccess(tokens, _) => tokens.concatenate
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseSexpr(p: String): LogicalTokens = parse(Config.sexpr, p)

  def parseDebug(p: String): LogicalTokens = parse(Config.debug, p)

  def parseWithoutLocation(p: String): LogicalTokens = parse(Config.withoutLocation, p)

  sealed trait Tokenizer {
  }

  trait SimpleTokenizer extends Tokenizer {
    def accept(context: Context, p: String, location: ParseLocation): Option[LogicalTokens] = accept_Token(context, p, location).map(LogicalTokens(_))
    protected def accept_Token(context: Context, p: String, location: ParseLocation): Option[LogicalToken] = None
  }

  trait ComplexTokenizer extends Tokenizer {
    def accept(context: Context, parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState]
  }

  case class Config(
    spaces: Vector[Char],
    delimiters: Vector[Char],
    comments: Vector[Char],
    cComment: Boolean,
    cppComment: Boolean,
    useFirstSpaceForComment: Boolean, // avoid confliction with path expression.
    simpleTokenizers: Vector[SimpleTokenizer],
    complexTokenizers: Vector[ComplexTokenizer],
    useSingleQuoteToken: Boolean,
    useAtom: Boolean,
    isDebug: Boolean,
    isLocation: Boolean
  ) extends ParseConfig {
    private val _simple_tokenizers = simpleTokenizers.toStream
    private val _complex_tokenizers = complexTokenizers.toStream

    def isSpace(c: Char): Boolean = spaces.contains(c)

    def isDelimiter(c: Char): Boolean = delimiters.contains(c)

    def isComment(evt: CharEvent): Boolean = comments.contains(evt.c)

    def isCComment(evt: CharEvent): Boolean = evt.c == '/' && evt.next == Some('*') && _first_char_for_comment(evt.next2)

    def isCppComment(evt: CharEvent): Boolean = evt.c == '/' && evt.next == Some('/') && _first_char_for_comment(evt.next2)

    def handle(context: Context, parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] =
      _complex_tokenizers.flatMap(_.accept(context, parent, evt)).headOption

    def getTokens(context: Context, p: String, l: ParseLocation): Option[LogicalTokens] = _simple_tokenizers.flatMap(_.accept(context, p, l)).headOption

    private def _first_char_for_comment(p: Option[Char]): Boolean =
      p.fold(true)(spaces.contains)
  }
  object Config {
    val default = Config(
      " \t\n\r\f".toVector,
      // "(){}[]".toVector, // "(){}[],".toVector, get rid of ',' for expression token.
      "(){}[],".toVector, // SExpression handles ',' as delimiter.
      Vector.empty, // ";".toVector, use c and c++ style comment instead.
      true,
      true,
      true,
      Vector(
        NumberToken, NumberPostfixToken,
        ComplexToken, RationalToken,
        IntervalToken, RangeToken,
        LocalDateToken, LocalDateTimeToken, LocalTimeToken, DateTimeToken, MonthDayToken,
        DateTimeIntervalToken, LocalDateTimeIntervalToken,
        PeriodToken, DurationToken,
        LxsvToken, XsvToken,
        UrlToken, UrnToken, UriToken,
        PathToken, ExpressionToken
      ),
      Vector(JsonParser(), XmlParser()),
      false,
      true,
      false,
      true
    )
    val withoutLocation = default.copy(isLocation = false)
    val sexpr = default.copy(useSingleQuoteToken = true)
    val debug = default.copy(isDebug = true)
    val xsv = default.copy(
      "".toVector,
      " \t,;&".toVector,
      useAtom = false
    )
    val csv = xsv.copy(
      "".toVector,
      ",".toVector
    )
    val tsv = xsv.copy(
      "".toVector,
      "\t".toVector
    )
    val scsv = xsv.copy(
      "".toVector,
      ";".toVector
    )
    val ssv = xsv.copy(
      "".toVector,
      " \t".toVector
    )
    val urlEncoding = default.copy(
      "".toVector,
      "&".toVector
    )
  }

  case class Context(
    config: Config,
    dateTimeContext: DateTimeContext
  ) extends ForwardParseConfig {
    def useSingleQuoteToken: Boolean = config.useSingleQuoteToken
    def isDebug = config.isDebug
    def isSpace(c: Char): Boolean = config.isSpace(c)
    def isDelimiter(c: Char): Boolean = config.isDelimiter(c)
    def isComment(evt: CharEvent): Boolean = config.isComment(evt)
    def isCComment(evt: CharEvent): Boolean = config.isCComment(evt)
    def isCppComment(evt: CharEvent): Boolean = config.isCppComment(evt)
    def handle(context: Context, parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] =
      config.handle(context, parent, evt)
    def getTokens(context: Context, p: String, l: ParseLocation): Option[LogicalTokens] =
      config.getTokens(context, p, l)
  }
  object Context {
    def create(): Context = create(Config.default)
    def create(config: Config): Context = Context(config, DateTimeContext.now)
  }

  trait LogicalTokensParseState extends ParseReaderWriterState[Context, LogicalTokens] {
    protected def use_Tokenizer = true
    protected def use_Delimiter = true
    protected def use_Double_Quote = true
    protected def use_Single_Quote = true
    protected def use_Bracket = true
    protected def use_Dollar(c: Context, p: CharEvent) = true
    protected def use_Comment = true

    def apply(context: Context, evt: ParseEvent): Transition = {
      if (context.isDebug)
        println(s"IN ($this): $evt")
      val r = handle_event(context, evt)
      if (context.isDebug)
        println(s"OUT ($this): $r")
      r
    }

    def addChildState(context: Context, p: Vector[Char]): LogicalTokensParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildState(context: Context, p: LogicalToken): LogicalTokensParseState =
      addChildState(context, LogicalTokens(p))

    def addChildState(context: Context, p: LogicalTokens): LogicalTokensParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildTransition(context: Context, p: LogicalTokens): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildTransition(context: Context, p: LogicalToken, evt: ParseEvent): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildEndTransition(context: Context, p: LogicalToken): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")

    def addChildEndTransition(context: Context, p: LogicalTokens): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")

    protected final def handle_event(context: Context, evt: ParseEvent): Transition =
      evt match {
        case StartEvent => handle_start(context)
        case EndEvent => handle_end(context)
        case m: LineEndEvent => handle_line_end(context, m)
        case m: CharEvent => handle_tokenizer(context, m) getOrElse {
          m.c match {
            case '"' if use_Double_Quote => handle_double_quote(context, m)
            case '\'' if use_Single_Quote => handle_single_quote(context, m)
            case '[' if use_Bracket => handle_open_bracket(context, m)
            case ']' if use_Bracket => handle_close_bracket(context, m)
            case '$' if use_Dollar(context, m) => handle_dollar(context, m)
            case '/' if use_Comment && context.isCComment(m) => handle_c_comment(context, m)
            case '/' if use_Comment && context.isCppComment(m) => handle_cpp_comment(context, m)
            case c if context.isSpace(c) => handle_space(context, m)
            case c if use_Delimiter && context.isDelimiter(c) => handle_delimiter(context, m)
            case c if use_Comment && context.isComment(m) => handle_comment(context, m) // TODO handle not " ;" (e.g. "id:1;name:taro")
            case _ => handle_character(context, m)
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

    protected final def handle_tokenizer(context: Context, evt: CharEvent): Option[Transition] =
      if (use_Tokenizer)
        context.handle(context, this, evt).map(x =>
          // (ParseMessageSequence.empty, end_Result(config), x)
          (ParseMessageSequence.empty, ParseResult.empty, x)
        )
      else
        None

    protected final def handle_start(context: Context): Transition =
      handle_Start(context)

    protected def handle_Start(context: Context): Transition =
      (ParseMessageSequence.empty, start_Result(context), start_State(context))

    protected def start_Result(context: Context): ParseResult[LogicalTokens] = 
      ParseSuccess(LogicalTokens.empty)

    protected def start_State(context: Context): LogicalTokensParseState = this

    protected final def handle_end(context: Context): Transition =
      handle_End(context)

    protected def handle_End(context: Context): Transition =
      (ParseMessageSequence.empty, end_Result(context), end_State(context))

    protected def end_Result(context: Context): ParseResult[LogicalTokens] =
      ParseSuccess(LogicalTokens.empty)

    protected def end_State(context: Context): LogicalTokensParseState = EndState

    protected final def handle_line_end(context: Context, evt: LineEndEvent): Transition =
      handle_Line_End(context, evt)

    protected def handle_Line_End(context: Context, evt: LineEndEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, line_End_State(context, evt))

    protected def line_End_State(context: Context, evt: LineEndEvent): LogicalTokensParseState = RAISE.notImplementedYetDefect(s"line_End_State(${getClass.getSimpleName})")

    protected def handle_space(context: Context, evt: CharEvent): Transition =
      handle_Space(context, evt)

    protected def handle_Space(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, space_State(context, evt))

    protected def space_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected def handle_delimiter(context: Context, evt: CharEvent): Transition =
      handle_Delimiter(context, evt)

    protected def handle_Delimiter(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, delimiter_State(context, evt))

    protected def delimiter_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected def handle_comment(context: Context, evt: CharEvent): Transition =
      handle_Comment(context, evt)

    protected def handle_Comment(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, comment_State(context, evt))

    protected def comment_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      EndState

    protected def handle_c_comment(context: Context, evt: CharEvent): Transition =
      handle_C_Comment(context, evt)

    protected def handle_C_Comment(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, comment_C_State(context, evt))

    protected def comment_C_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      RAISE.notImplementedYetDefect(s"comment_C_State(${getClass.getSimpleName})")

    protected def handle_cpp_comment(context: Context, evt: CharEvent): Transition =
      handle_Cpp_Comment(context, evt)

    protected def handle_Cpp_Comment(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, comment_Cpp_State(context, evt))

    protected def comment_Cpp_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      RAISE.notImplementedYetDefect(s"comment_Cpp_State(${getClass.getSimpleName})")

    protected final def handle_double_quote(context: Context, evt: CharEvent): Transition =
      handle_Double_Quote(context, evt)

    protected def handle_Double_Quote(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, double_Quote_State(context, evt))

    protected def double_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected final def handle_single_quote(context: Context, evt: CharEvent): Transition =
      handle_Single_Quote(context, evt)

    protected def handle_Single_Quote(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, single_Quote_State(context, evt))

    protected def single_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected final def handle_open_bracket(context: Context, evt: CharEvent): Transition =
      handle_Open_Bracket(context, evt)

    protected def handle_Open_Bracket(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, open_Bracket_State(context, evt))

    protected def open_Bracket_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected final def handle_close_bracket(context: Context, evt: CharEvent): Transition =
      handle_Close_Bracket(context, evt)

    protected def handle_Close_Bracket(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, close_Bracket_State(context, evt))

    protected def close_Bracket_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected final def handle_dollar(context: Context, evt: CharEvent): Transition =
      handle_Dollar(context, evt)

    protected def handle_Dollar(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, dollar_State(context, evt))

    protected def dollar_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(context, evt)

    protected final def handle_character(context: Context, evt: CharEvent): Transition =
      handle_Character(context, evt)

    protected def handle_Character(context: Context, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, character_State(context, evt))

    protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      character_State(evt.c)

    protected def character_State(c: Char): LogicalTokensParseState =
      RAISE.notImplementedYetDefect(s"character_State(${getClass.getSimpleName}): $c[${c.toInt}]")
    //
    protected final def to_tokens(context: Context, p: Vector[Char], l: ParseLocation): LogicalTokens = to_tokens(context, p.mkString, l)

    protected final def to_tokens(context: Context, p: String, l: ParseLocation): LogicalTokens =
      context.getTokens(context, p, l) getOrElse LogicalTokens(_to_atom_or_string(context, p, l))

    private def _to_atom_or_string(context: Context, p: String, l: ParseLocation) =
      if (context.config.useAtom)
        AtomToken(p, l)
      else
        RawStringToken(p, l)

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
    override protected val use_Tokenizer = false
    override protected val use_Delimiter = false
    override protected val use_Double_Quote = false
    override protected val use_Single_Quote = false
    override protected val use_Bracket = false
    override protected def use_Dollar(c: Context, p: CharEvent) = false
  }

  trait ChildLogicalTokensParseState extends LogicalTokensParseState {
    def parent: LogicalTokensParseState

    protected final def flush_state(context: Context, p: LogicalTokens): LogicalTokensParseState =
      flush_State(context, p)

    protected final def flush_state(context: Context, p: LogicalToken): LogicalTokensParseState =
      flush_State(context, p)

    protected final def flush_state(context: Context): LogicalTokensParseState =
      flush_State(context)

    protected final def flush_tokens(context: Context): LogicalTokens =
      flush_Tokens(context)

    protected final def flush_tokens(context: Context, p: LogicalTokens): LogicalTokens =
      flush_Tokens(context, p)

    protected def flush_State(context: Context, p: LogicalTokens): LogicalTokensParseState =
      parent.addChildState(context, flush_tokens(context, p))

    protected def flush_State(context: Context, p: LogicalToken): LogicalTokensParseState =
      flush_State(context, LogicalTokens(p))

    protected def flush_State(context: Context): LogicalTokensParseState =
      flush_State(context, LogicalTokens.empty)

    protected def flush_Tokens(context: Context): LogicalTokens

    protected def flush_Tokens(context: Context, p: LogicalTokens): LogicalTokens =
      flush_Tokens(context) + p

    override def addChildState(context: Context, p: LogicalTokens): LogicalTokensParseState =
      flush_state(context, p)

    override def addChildTransition(context: Context, p: LogicalToken, evt: ParseEvent): Transition =
      flush_state(context, p).apply(context, evt)

    override def addChildEndTransition(context: Context, p: LogicalToken): Transition =
      flush_state(context, p).apply(context, EndEvent)

    override def addChildEndTransition(context: Context, p: LogicalTokens): Transition =
      flush_state(context, p).apply(context, EndEvent)

    // override protected def end_Result(context: Context): ParseResult[LogicalTokens] =
    //   ParseSuccess(flush_tokens(config))

    override protected def handle_End(context: Context): Transition =
      parent.addChildEndTransition(context, flush_tokens(context))
  }

  // can use terminal character to handle token end.
  trait WithTerminalLogicalTokensParseState extends LogicalTokensParseState {
    override protected val use_Tokenizer = false
    override protected val use_Delimiter = false
    override protected val use_Double_Quote = false
    override protected val use_Single_Quote = false
    override protected val use_Bracket = false
    // override val use_Dollar = false ???
  }

  // can't use terminal character to handle token end, use delimiter instead.
  trait WithoutTerminalLogicalTokensParseState extends ChildLogicalTokensParseState {
    def parent: LogicalTokensParseState

    override protected val use_Tokenizer = false
    override protected val use_Delimiter = true
    override protected val use_Double_Quote = false
    override protected val use_Single_Quote = false
    override protected val use_Bracket = false
    // override val use_Dollar = false ???

    override protected def space_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      flush_state(context)

    override protected def delimiter_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      flush_state(context, DelimiterToken(evt.c, evt.location))
  }

  trait StringLiteralLogicalTokensParseState extends LogicalTokensParseState {
    override protected def double_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.next == Some('"') && evt.next2 == Some('"'))
        SkipState(SkipState(RawStringState(this, evt)))
      else
        DoubleQuoteState(this, evt)

    override protected def single_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      SingleQuoteState(this, evt)
  }

  case object EndState extends LogicalTokensParseState {
    override def apply(context: Context, evt: ParseEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, this)
  }

  case class SkipState(next: Transition) extends LogicalTokensParseState {
    override def apply(context: Context, evt: ParseEvent): Transition = next
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
    override def addChildState(context: Context, p: LogicalTokens): LogicalTokensParseState =
      parent.addChildState(context, SpaceToken(spaces.mkString, None) +: p)

    override protected def handle_End(context: Context): Transition =
      parent.addChildEndTransition(context, SpaceToken(spaces.mkString, location))

    override protected def handle_Double_Quote(context: Context, evt: CharEvent): Transition =
      handle_character(context, evt)

    override protected def handle_Single_Quote(context: Context, evt: CharEvent): Transition =
      handle_character(context, evt)

    override protected def handle_Character(context: Context, evt: CharEvent): Transition =
      if (context.isSpace(evt.c))
        (ParseMessageSequence.empty, ParseResult.empty, space_State(context, evt))
      else
        parent.addChildTransition(context, SpaceToken(spaces.mkString, location), evt)

    override protected def space_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      copy(spaces = spaces :+ evt.c)
  }

  object SpaceState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): SpaceState =
      SpaceState(p, Vector(evt.c), evt.location.toOption)
  }

  case class CommentState(
    parent: LogicalTokensParseState
  ) extends LogicalTokensParseState {
    // TODO end mark
  }

  trait CommentStateBase extends ChildLogicalTokensParseState {
    override protected def use_Tokenizer = false
    override protected def use_Delimiter = false
    override protected def use_Single_Quote = false
    override protected def use_Bracket = false
    override protected def use_Dollar(c: Context, p: CharEvent) = false
    override protected def use_Comment = false

    protected def to_Token(): CommentToken

    protected def flush_Tokens(context: Context): LogicalTokens = RAISE.noReachDefect

    override protected def handle_End(context: Context): Transition =
      parent.addChildEndTransition(context, to_Token())

    override protected def handle_Line_End(context: Context, evt: LineEndEvent): Transition =
      parent.addChildTransition(context, to_Token(), evt)
  }

  case class CCommentState(
    parent: LogicalTokensParseState,
    location: Option[ParseLocation],
    comment: Vector[Char] = Vector.empty
  ) extends CommentStateBase {
    protected def to_Token() = CommentToken(comment.mkString, location)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      if (evt.c == '*' && evt.next == Some('/'))
        SkipState(parent.addChildState(context, to_Token()))
      else
        copy(comment = comment :+ evt.c)
    }
  }

  case class CppCommentState(
    parent: LogicalTokensParseState,
    location: Option[ParseLocation],
    comment: Vector[Char] = Vector.empty
  ) extends CommentStateBase {
    protected def to_Token() = CommentToken(comment.mkString, location)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      if (evt.c == '\n')
        parent.addChildState(context, to_Token())
      else if (evt.c == '\r' && evt.next == Some('\n'))
        SkipState(parent.addChildState(context, to_Token()))
      else if (evt.c == '\r')
        parent.addChildState(context, to_Token())
      else
        copy(comment = comment :+ evt.c)
    }
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
    override protected def use_Dollar(c: Context, p: CharEvent) = false
    override protected def use_Comment = false

    private def _is_raw = prefix.fold(false)(_ == "raw")
    private def _is_not_raw = !_is_raw

    override def addChildState(context: Context, p: Vector[Char]): LogicalTokensParseState =
      copy(text = text ++ p)

    override protected def double_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      // println(s"DoubleQuote#double_Quote_State: $evt")
      parent.addChildState(context, DoubleStringToken(text.mkString, location, prefix))
    }

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      // println(s"DoubleQuoteState#double_Quote_State: $evt")
      if (evt.c == '\\' && _is_not_raw)
        StringQuoteEscapeState(this)
      else
        copy(text = text :+ evt.c)
    }
  }
  object DoubleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DoubleQuoteState =
      DoubleQuoteState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DoubleQuoteState =
      DoubleQuoteState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class StringQuoteEscapeState(
    parent: LogicalTokensParseState
  ) extends LogicalTokensParseState {
    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      evt.c match {
        case '\\' => parent.addChildState(context, Vector('\\'))
        case '"' => parent.addChildState(context, Vector('"'))
        case '\'' => parent.addChildState(context, Vector('\''))
        case 't' => parent.addChildState(context, Vector('\t'))
        case 'f' => parent.addChildState(context, Vector('\f'))
        case 'n' => parent.addChildState(context, Vector('\n'))
        case 'r' => parent.addChildState(context, Vector('\r'))
        case c => parent.addChildState(context, Vector(c))
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
    override protected def use_Dollar(c: Context, p: CharEvent) = false
    override protected def use_Comment = false

    override def addChildState(context: Context, p: Vector[Char]): LogicalTokensParseState =
      copy(text = text ++ p)

    override protected def single_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(context, SingleStringToken(text.mkString, location))

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '\\')
        StringQuoteEscapeState(this)
      else
        copy(text = text :+ evt.c)
  }
  object SingleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): SingleQuoteState =
      SingleQuoteState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: String): SingleQuoteState =
      SingleQuoteState(p, Vector.empty, evt.location.toOption, Some(prefix))
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
    override protected def use_Dollar(c: Context, p: CharEvent) = false
    override protected def use_Comment = false

    override protected def line_End_State(context: Context, evt: LineEndEvent): LogicalTokensParseState =
      parent.addChildState(context, RawStringToken(text.mkString, location, prefix))

    override protected def double_Quote_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      // println(s"RawStringState#double_Quote_State: $evt")
      if (evt.c == '"' && evt.next == Some('"') && evt.next2 == Some('"')) {
        if (inClosing)
          copy(text = text :+ evt.c)
        else
          copy(inClosing = true)
      } else if (evt.c == '"' && evt.next == Some('"')) {
        if (inClosing)
          SkipState(parent.addChildState(context, RawStringToken(text.mkString, location, prefix)))
        else
          copy(text = text :+ evt.c)
      } else {
        copy(text = text :+ evt.c)
      }
    }

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      // println(s"RawStringState#character_State: $evt")
      copy(text = text :+ evt.c)
    }
  }
  object RawStringState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): RawStringState =
      RawStringState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): RawStringState =
      RawStringState(p, Vector.empty, evt.location.toOption, prefix)
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

    override protected def close_Bracket_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(context, _token)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object BracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): BracketState =
      BracketState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): BracketState =
      BracketState(p, Vector.empty, Some(evt.location.adjustPrefix(prefix)), prefix)
  }

  case class DoubleBracketState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def close_Bracket_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("]]"))
        SkipState(parent.addChildState(context, DoubleBracketToken(text.mkString, location, prefix)))
      else
        copy(text = text :+ evt.c)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object DoubleBracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DoubleBracketState =
      DoubleBracketState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DoubleBracketState =
      DoubleBracketState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class RawBracketState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends LogicalTokensParseState {
    override protected def close_Bracket_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("]]]"))
        SkipState(SkipState(parent.addChildState(context, RawBracketToken(text.mkString, location, prefix))))
      else
        copy(text = text :+ evt.c)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object RawBracketState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): RawBracketState =
      RawBracketState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): RawBracketState =
      RawBracketState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class DollarState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String]
  ) extends RawLogicalTokensParseState {
    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = 
      if (evt.c == '{')
        ScriptState(parent, Vector.empty, location, prefix)
      else if (evt.c == '[')
        ScriptPropertiesState(parent, Vector.empty, location, prefix)
      else
        ShortScriptState(parent, Vector(evt.c), location, prefix)
  }
  object DollarState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DollarState =
      DollarState(p, Vector.empty, evt.location.toOption, None)
    def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): DollarState =
      DollarState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class ShortScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String] = None
  ) extends WithoutTerminalLogicalTokensParseState {
    // protected def flush_State(context: Context, p: LogicalTokens): LogicalTokensParseState = {
    //   parent.addChildState(config, flush_tokens(config))
    // }

    protected def flush_Tokens(context: Context): LogicalTokens =
      LogicalTokens(ScriptToken(text.mkString, location, prefix, properties))

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '%')
        ScriptPostfixState(parent, text.mkString, location, prefix, properties)
      else
        copy(text = text :+ evt.c)
  }
  object ShortScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ShortScriptState =
    //   ShortScriptState(p, Vector.empty, evt.location.toOption, None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ShortScriptState =
    //   ShortScriptState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class ScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String] = None
  ) extends WithTerminalLogicalTokensParseState {
    // protected def flush_State(context: Context, p: LogicalTokens): LogicalTokensParseState = {
    //   parent.addChildState(config, flush_tokens(config))
    // }

    // protected def flush_Tokens(context: Context): LogicalTokens =
    //   LogicalTokens(ScriptToken(text.mkString, location))

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = 
      if (evt.isMatch("{{"))
        SkipState(ExtraScriptState(parent, Vector.empty, location, prefix, properties))
      else
        LongScriptState(parent, evt.c, location, prefix, properties)
  }
  object ScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ScriptState =
    //   ScriptState(p, Vector.empty, evt.location.toOption, None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ScriptState =
    //   ScriptState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class LongScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String],
    postfix: Option[String] = None,
    count: Int = 1
  ) extends WithTerminalLogicalTokensParseState {
    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      val c = evt.c
      // println(s"LongScriptState $c, $count")
      c match {
        case '{' => copy(count = count + 1, text = text :+ c)
        case '}' => 
          count - 1 match {
            case 0 =>
              evt.next.collect {
                case '%' => SkipState(ScriptPostfixState(parent, text.mkString, location, prefix, properties))
              }.getOrElse(
                parent.addChildState(context, ScriptToken(text.mkString, location, prefix, properties))
              )
            case n => copy(count = n, text = text :+ c)
          }
        case _ => copy(text = text :+ c)
      }
    }
  }
  object LongScriptState {
    def apply(
      p: LogicalTokensParseState,
      c: Char,
      location: Option[ParseLocation],
      prefix: Option[String],
      properties: Option[String]
    ): LongScriptState = {
      val n = c match {
        case '{' => 2
        case _ => 1
      }
      LongScriptState(p, Vector(c), location, prefix, properties, None, n)
    }
  }

  case class ExtraScriptState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String]
  ) extends WithTerminalLogicalTokensParseState {
    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.isMatch("}}}"))
        SkipState(SkipState(parent.addChildState(context, ScriptToken(text.mkString, location, prefix, properties))))
      else
        copy(text = text :+ evt.c)
  }
  object ExtraScriptState {
    // def apply(p: LogicalTokensParseState, evt: CharEvent): ExtraScriptState =
    //   ExtraScriptState(p, Vector.empty, evt.location.toOption, None)
    // def apply(p: LogicalTokensParseState, evt: CharEvent, prefix: Option[String]): ExtraScriptState =
    //   ExtraScriptState(p, Vector.empty, evt.location.toOption, prefix)
  }

  case class ScriptPropertiesState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Vector[Char] = Vector.empty
  ) extends WithTerminalLogicalTokensParseState {
    private def _get_properties = if (properties.isEmpty) None else Some(properties.mkString)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == ']') // XXX skip "{"
        SkipState(ScriptState(parent, text, location, prefix, _get_properties))
      else
        copy(properties = properties :+ evt.c)
  }

  case class ScriptPostfixState(
    parent: LogicalTokensParseState,
    text: String,
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String]
  ) extends WithTerminalLogicalTokensParseState {
    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '{')
        ScriptLongPostfixState(parent, text, location, prefix, properties)
      else
        ScriptShortPostfixState(parent, text, location, prefix, properties, Vector(evt.c))
  }

  case class ScriptLongPostfixState(
    parent: LogicalTokensParseState,
    text: String,
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String],
    postfix: Vector[Char] = Vector.empty,
    count: Int = 1
  ) extends WithTerminalLogicalTokensParseState {
    private def _get_postfix = if (postfix.isEmpty) None else Some(postfix.mkString)

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState = {
      val c = evt.c
      c match {
        case '{' => copy(count = count + 1, postfix = postfix :+ c)
        case '}' =>
          count - 1 match {
            case 0 => parent.addChildState(context, ScriptToken(text, location, prefix, properties, _get_postfix))
            case n => copy(count = n, text = text :+ c)
          }
        case _ => copy(postfix = postfix :+ c)
      }
    }
  }

  case class ScriptShortPostfixState(
    parent: LogicalTokensParseState,
    text: String,
    location: Option[ParseLocation],
    prefix: Option[String],
    properties: Option[String],
    postfix: Vector[Char],
    count: Int = 0
  ) extends WithoutTerminalLogicalTokensParseState {
    private def _get_postfix = if (postfix.isEmpty) None else Some(postfix.mkString)

    protected def flush_Tokens(context: Context): LogicalTokens =
      LogicalTokens(ScriptToken(text.mkString, location, prefix, properties, _get_postfix))

    override protected def character_State(context: Context, evt: CharEvent): LogicalTokensParseState =
      copy(postfix = postfix :+ evt.c)
  }

  case class UrnOrLxsvState(
    parent: LogicalTokensParseState,
    cs: Vector[Char],
    location: ParseLocation,
    isLxsv: Option[Boolean] = None
  ) extends ChildLogicalTokensParseState {
    override protected val use_Tokenizer = true
    override protected val use_Delimiter = true
    override protected val use_Double_Quote = true
    override protected val use_Single_Quote = false
    override protected val use_Bracket = true
    override protected def use_Dollar(c: Context, p: CharEvent) = false

    def tokens(context: Context): LogicalTokens = isLxsv.collect {
      case true => LogicalTokens(LxsvToken(cs.mkString, location))
    }.getOrElse(to_tokens(context, cs.mkString, location))

    protected def flush_Tokens(context: Context): LogicalTokens = tokens(context)

    override def addChildState(context: Context, p: LogicalToken): LogicalTokensParseState =
      p match {
        case m: StringToken => copy(cs = cs ++ m.text, isLxsv = Some(true))
        case m => RAISE.noReachDefect
      }

    // private def _flush(config: Config): UrnOrLxsvState = this

    // private def _flush(config: Config, t: LogicalToken): UrnOrLxsvState =
    //   copy(cs = cs ++ t.raw)

    // private def _flush(config: Config, t: LogicalTokens): UrnOrLxsvState =
    //   copy(cs = cs ++ t.raw)

    // override def addChildState(config: Config, p: LogicalTokens): LogicalTokensParseState =
    //   _flush(config, p)

    // override def addChildTransition(config: Config, p: LogicalToken, evt: CharEvent): Transition =
    //   (ParseMessageSequence.empty, ParseResult.empty, _flush(config, p))

    // override def addChildEndTransition(config: Config, p: LogicalToken): Transition =
    //   _flush(config, p).apply(config, EndEvent)

    // override protected def end_Result(config: Config): ParseResult[LogicalTokens] =
    //   ParseSuccess(_flush(config).tokens(config))

    // override protected def space_State(config: Config, evt: CharEvent) =
    //   SpaceState(this, evt)

    override protected def handle_delimiter(context: Context, evt: CharEvent) =
      if (cs.isEmpty)
        RAISE.noReachDefect
      else
        parent.addChildState(context, tokens(context)).apply(context, evt)

    // override protected def delimiter_State(config: Config, evt: CharEvent) = {
    //   val a = if (cs.isEmpty) {
    //     LogicalTokens(DelimiterToken(evt.c, evt.location))
    //   } else {
    //     tokens(config) + DelimiterToken(evt.c, evt.location)
    //   }
    //   val l = evt.location
    //   copy(cs = Vector.empty, location = l)
    // }

    override protected def double_Quote_State(context: Context, evt: CharEvent) = {
      if (evt.isMatch("\"\"\""))
        SkipState(SkipState(RawStringState(this, evt)))
      else
        DoubleQuoteState(this, evt)
    }

    override protected def space_State(context: Context, evt: CharEvent) =
      SpaceState(parent.addChildState(context, tokens(context)), evt)

    override protected def character_State(context: Context, evt: CharEvent) =
      copy(cs = cs :+ evt.c)
  }

  case class NormalState(
    cs: Vector[Char],
    tokens: LogicalTokens,
    location: ParseLocation
  ) extends StringLiteralLogicalTokensParseState {
    override protected def use_Dollar(c: Context, p: CharEvent) = p.next.fold(false)(x => !c.isSpace(x))

    private def _flush(context: Context): NormalState = {
      if (cs.isEmpty)
        this
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(context, cs.mkString, location))
    }

    private def _flush(context: Context, t: LogicalToken): NormalState = {
      if (cs.isEmpty)
        copy(tokens = tokens + t)
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(context, cs.mkString, location) + t)
    }

    private def _flush(context: Context, t: LogicalTokens): NormalState = {
      if (cs.isEmpty)
        copy(tokens = tokens + t)
      else
        copy(cs = Vector.empty, tokens = tokens + to_tokens(context, cs.mkString, location) + t)
    }

    override def addChildState(context: Context, p: LogicalTokens): LogicalTokensParseState =
      _flush(context, p)

    override def addChildTransition(context: Context, p: LogicalToken, evt: ParseEvent): Transition =
      _flush(context, p).apply(context, evt)

    override def addChildEndTransition(context: Context, p: LogicalToken): Transition =
      _flush(context, p).apply(context, EndEvent)

    override def addChildEndTransition(context: Context, p: LogicalTokens): Transition =
      _flush(context, p).apply(context, EndEvent)

    override protected def end_Result(context: Context): ParseResult[LogicalTokens] =
      ParseSuccess(_flush(context).tokens)

    override protected def space_State(context: Context, evt: CharEvent) =
      SpaceState(this, evt)

    override protected def delimiter_State(context: Context, evt: CharEvent) = {
      val a = if (cs.isEmpty) {
        LogicalTokens(DelimiterToken(evt.c, evt.location))
      } else {
        val b = to_tokens(context, cs.mkString, location)
        b + DelimiterToken(evt.c, evt.location)
      }
      val l = evt.location
      copy(Vector.empty, tokens + a, l)
    }

    override protected def comment_C_State(context: Context, evt: CharEvent) =
      if (cs.isEmpty) {
        SkipState(CCommentState(this, Some(evt.location)))
      } else {
        val newparent = copy(cs = Vector.empty, tokens = tokens + to_tokens(context, cs.mkString, location))
        SkipState(CCommentState(newparent, Some(evt.location)))
      }

    override protected def comment_Cpp_State(context: Context, evt: CharEvent) =
      if (cs.isEmpty) {
        SkipState(CppCommentState(this, Some(evt.location)))
      } else {
        val newparent = copy(cs = Vector.empty, tokens = tokens + to_tokens(context, cs.mkString, location))
        SkipState(CppCommentState(newparent, Some(evt.location)))
      }

    override protected def double_Quote_State(context: Context, evt: CharEvent) = {
      // println(s"NormalState#double_Quote_State: $evt")
      // println(s"""NormalState#double_Quote_State - ${evt.isMatch("\"\"\"")}""")
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      if (evt.isMatch("\"\"\""))
        SkipState(SkipState(RawStringState(newparent, evt, prefix)))
      else
        DoubleQuoteState(newparent, evt, prefix)
    }

    override protected def single_Quote_State(context: Context, evt: CharEvent) =
      if (context.useSingleQuoteToken) {
        _flush(context, SingleQuoteToken(evt.location.toOption))
      } else {
        val prefix = cs.mkString
        val newparent = copy(cs = Vector.empty)
        SingleQuoteState(newparent, evt, prefix)
      }

    override protected def open_Bracket_State(context: Context, evt: CharEvent) = {
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      if (evt.isMatch("[[["))
        SkipState(SkipState(RawBracketState(newparent, evt, prefix)))
      else if (evt.isMatch("[["))
        SkipState(DoubleBracketState(newparent, evt, prefix))
      else
        BracketState(newparent, evt, prefix)
    }

    override protected def dollar_State(context: Context, evt: CharEvent) = {
      val prefix = if (cs.isEmpty) None else Some(cs.mkString)
      val newparent = copy(cs = Vector.empty)
      DollarState(newparent, evt, prefix)
    }

    override protected def character_State(context: Context, evt: CharEvent) = {
      val c = evt.c
      c match {
        case ':' =>
          val parent = copy(cs = Vector.empty)
          UrnOrLxsvState(parent, cs :+ c, location)
        case _ => 
          if (cs.isEmpty)
            copy(cs = cs :+ c, location = evt.location)
          else
            copy(cs = cs :+ c)
      }
    }
  }
  object NormalState {
    val init = NormalState(Vector.empty, LogicalTokens.empty, ParseLocation.start)
  }

  case class PartialParserStartState() extends LogicalTokensParseState {
    override def addChildState(context: Context, p: LogicalTokens): LogicalTokensParseState = {
      PartialParserEndState(p.makeToken)
    }
  }

  case class PartialParserEndState(
    token: LogicalToken
  ) extends LogicalTokensParseState {
    // def result: (LogicalToken, CharSequence) = ???
  }

  case class PartialParserStateMachine(
    context: Context,
    state: LogicalTokensParseState
  ) {
    def apply(c: Char): \/[PartialParserStateMachine, LogicalToken] = {
      val (m, r, s) = state.apply(context, CharEvent(c))
      s match {
        case m: PartialParserEndState => \/-(m.token)
        case _ => -\/(copy(state = s))
      }
    }

    def getResult: Option[(LogicalToken, CharSequence)] = RAISE.notImplementedYetDefect
  }
  object PartialParserStateMachine {
    def apply(context: Context): PartialParserStateMachine = PartialParserStateMachine(
      context,
      PartialParserStartState()
    )
  }

  def parseHead(ps: CharSequence): (LogicalToken, CharSequence) = parseHead(Context.create(), ps)

  def parseHead(context: Context, ps: CharSequence): (LogicalToken, CharSequence) = {
    var sm = PartialParserStateMachine(context)

    for (i <- 0 until ps.length) {
//      sm = sm.apply(ps.charAt(i))
      // sm.getResult match {
      //   case Some(s) => return s
      //   case None => // do nothing
      // }
      RAISE.notImplementedYetDefect
    }
    RAISE.syntaxErrorFault("LogicalTokens.parseHead")
  }
}
