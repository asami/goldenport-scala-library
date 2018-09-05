package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 28, 2018
 *  version Aug. 29, 2018
 * @version Sep.  2, 2018
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
    val parser = ParseReaderWriterStateClass[Config, LogicalTokens](Config.default, NormalState.init)
    val (messages, result, state) = parser.apply(p)
    result match {
      case ParseSuccess(tokens, _) => tokens
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  sealed trait Tokenizer {
  }

  trait SimpleTokenizer extends Tokenizer {
    def accept(p: String): Option[LogicalTokens]
  }

  trait ComplexTokenizer extends Tokenizer {
    def accept(parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState]
  }

  case class Config(
    spaces: Vector[Char],
    delimiters: Vector[Char],
    comments: Vector[Char],
    simpleTokenizers: Vector[SimpleTokenizer],
    complexTokenizers: Vector[ComplexTokenizer],
    isDebug: Boolean = false
  ) extends ParseConfig {
    private val _simple_tokenizers = simpleTokenizers.toStream
    private val _complex_tokenizers = complexTokenizers.toStream

    def isSpace(c: Char): Boolean = spaces.contains(c)

    def isDelimiter(c: Char): Boolean = delimiters.contains(c)

    def isComment(evt: CharEvent): Boolean = comments.contains(evt.c)

    def handle(parent: LogicalTokensParseState, evt: CharEvent): Option[LogicalTokensParseState] =
      _complex_tokenizers.flatMap(_.accept(parent, evt)).headOption

    def getTokens(p: String): Option[LogicalTokens] = _simple_tokenizers.flatMap(_.accept(p)).headOption
  }
  object Config {
    val a: LogicalTokens.ComplexTokenizer = JsonParser()
    val default = Config(
      " \t\n\r\f".toVector,
      "(){}[],".toVector,
      ";".toVector,
      Vector(),
      Vector(JsonParser(), XmlParser())
    )
  }

  trait LogicalTokensParseState extends ParseReaderWriterState[Config, LogicalTokens] {
    protected def use_Tokenizer = true
    protected def use_Delimiter = true
    protected def use_Double_Quote = true
    protected def use_Single_Quote = true

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
        case EndEvent => handle_end(config)
        case m: LineEndEvent => handle_line_end(config, m)
        case m: CharEvent => handle_tokenizer(config, m) getOrElse {
          m.c match {
            case '"' if use_Double_Quote => handle_double_quote(config, m)
            case ''' if use_Single_Quote => handle_single_quote(config, m)
            case c if config.isSpace(c) => handle_space(config, m)
            case c if use_Delimiter && config.isDelimiter(c) => handle_delimiter(config, m)
            case c if config.isComment(m) => handle_comment(config, m)
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
          (ParseMessageSequence.empty, end_Result(config), x)
        )
      else
        None

    protected final def handle_end(config: Config): Transition =
      handle_End(config)

    protected def handle_End(config: Config): Transition =
      (ParseMessageSequence.empty, end_Result(config), end_State(config))

    protected def end_Result(config: Config): ParseResult[LogicalTokens] = 
      RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName})")

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


    protected final def handle_character(config: Config, evt: CharEvent): Transition =
      handle_Character(config, evt)

    protected def handle_Character(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, character_State(config, evt))

    protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      character_State(evt.c)

    protected def character_State(c: Char): LogicalTokensParseState =
      RAISE.notImplementedYetDefect(s"character_State(${getClass.getSimpleName}): $c")
    //
    protected final def to_tokens(config: Config, p: Vector[Char], l: ParseLocation): LogicalTokens = to_tokens(config, p.mkString, l)

    protected final def to_tokens(config: Config, p: String, l: ParseLocation): LogicalTokens =
      config.getTokens(p) getOrElse LogicalTokens(AtomToken(p, l))

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

  trait StringLiteralLogicalTokensParseState extends LogicalTokensParseState {
    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.next == Some("\"") && evt.next2 == Some("\""))
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
  ) extends LogicalTokensParseState {
    override val use_Delimiter = false
    override val use_Double_Quote = false
    override val use_Single_Quote = false

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
    location: Option[ParseLocation]
  ) extends LogicalTokensParseState {
    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(config, DoubleStringToken(text.mkString, location))

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '\\')
        RAISE.notImplementedYetDefect
      else
        copy(text = text :+ evt.c)
  }
  object DoubleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): DoubleQuoteState =
      DoubleQuoteState(p, Vector.empty, Some(evt.location))
  }

  case class SingleQuoteState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends LogicalTokensParseState {
    override protected def single_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      parent.addChildState(config, SingleStringToken(text.mkString, location))

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '\\')
        RAISE.notImplementedYetDefect
      else
        copy(text = text :+ evt.c)
  }
  object SingleQuoteState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): SingleQuoteState =
      SingleQuoteState(p, Vector.empty, Some(evt.location))
  }

  case class RawStringState(
    parent: LogicalTokensParseState,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends LogicalTokensParseState {
    override protected def double_Quote_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      if (evt.c == '"' && evt.next == Some('"') && evt.next2 == Some('"'))
        SkipState(SkipState(parent.addChildState(config, RawStringToken(text.mkString, location))))
      else
      copy(text = text :+ evt.c)

    override protected def character_State(config: Config, evt: CharEvent): LogicalTokensParseState =
      copy(text = text :+ evt.c)
  }
  object RawStringState {
    def apply(p: LogicalTokensParseState, evt: CharEvent): RawStringState =
      RawStringState(p, Vector.empty, Some(evt.location))
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
        val b = AtomToken(cs.mkString, location)
        LogicalTokens(b, DelimiterToken(evt.c, evt.location))
      }
      val l = evt.location
      copy(Vector.empty, tokens + a, l)
    }

    override protected def character_State(config: Config, evt: CharEvent) =
      if (cs.isEmpty)
        copy(cs = cs :+ evt.c, location = evt.location)
      else
        copy(cs = cs :+ evt.c)
  }
  object NormalState {
    val init = NormalState(Vector.empty, LogicalTokens.empty, ParseLocation.init)
  }
}