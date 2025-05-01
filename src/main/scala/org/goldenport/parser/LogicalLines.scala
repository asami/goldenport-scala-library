package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE
import org.goldenport.io.{InputSource, ResourceHandle}
import org.goldenport.i18n.I18NContext
import org.goldenport.collection.NonEmptyVector
import org.goldenport.util.StringUtils

/*
 * @since   Aug. 20, 2018
 *  version Sep. 29, 2018
 *  version Oct. 30, 2018
 *  version Dec. 31, 2018
 *  version Jan.  1, 2019
 *  version Feb. 16, 2019
 *  version May.  2, 2019
 *  version Jun. 30, 2019
 *  version Dec.  7, 2019
 *  version Jan. 31, 2020
 *  version Jan. 16, 2021
 *  version Feb.  7, 2021
 *  version Mar.  2, 2021
 *  version Apr.  3, 2021
 *  version May. 31, 2021
 *  version Jun.  9, 2024
 *  version Sep. 10, 2024
 *  version Oct. 31, 2024
 *  version Nov. 23, 2024
 *  version Jan.  1, 2025
 *  version Feb.  7, 2025
 * @version Apr.  6, 2025
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

  lazy val lineVector = lines.map(_.text)
  lazy val text = lines.map(_.text + "\n").mkString
}

object LogicalLines {
  type Transition = (ParseMessageSequence, ParseResult[LogicalLines], LogicalLinesParseState)

  implicit object LogicalLinesMonoid extends Monoid[LogicalLines] {
    def zero = empty
    def append(lhs: LogicalLines, rhs: => LogicalLines) = lhs + rhs
  }

  val empty = LogicalLines(Vector.empty)

  case class Slot(
    cs: Vector[Char],
    candidates: Vector[Char],
    lines: Vector[String]
  ) {
    def isEmpty = cs.isEmpty
    def lastOption = cs.lastOption
    def mkString = cs.mkString
    def phsicalLines: List[String] = if (candidates.isEmpty)
      lines.toList
    else
      (lines :+ candidates.mkString).toList
    def toLogicalLine(p: Option[ParseLocation]): LogicalLine = LogicalLine(cs.mkString, phsicalLines, p)
    def ++(ps: Seq[Char]) = copy(cs = cs ++ ps, candidates = candidates ++ ps)
    def :+(p: Char) = copy(cs = cs :+ p, candidates = candidates :+ p)
    def addPhysicalLineEnd(p: Char) = {
      val a = if (candidates.isEmpty)
        lines
      else
        lines :+ candidates.mkString
      copy(cs = cs :+ p, candidates = Vector.empty, lines = a)
    }
    def addMultiLine(p: String) = addPhysicalLineEnd('\n').++(p).addPhysicalLineEnd('\n')
  }
  object Slot {
    val empty = Slot(Vector.empty, Vector.empty, Vector.empty)

    def apply(p: Char): Slot = Slot(Vector(p), Vector(p), Vector.empty)
  }

  case class Builder(
    lines: Vector[LogicalLine] = Vector.empty,
    in: LogicalLine.Builder = LogicalLine.Builder()
  ) {
    def lastOption: Option[Char] = in.lastOption
    def getCurrentLine: Option[String] = in.getCurrentLine
    def currentLine: String = in.currentLine
    def location: Option[ParseLocation] = in.location

    def add(evt: CharEvent): Builder = copy(in = in.add(evt))
    def lineEnd(evt: CharEvent): Builder = {
      val s = in.build()
      copy(lines = lines ++ s.toVector, in = LogicalLine.Builder())
    }

    def build(): LogicalLines = LogicalLines(lines ++ in.build().toVector)
  }
  object Builder {
    def apply(p: String, location: Option[ParseLocation]): Builder =
      Builder(lines = Vector(LogicalLine(p, location)))
  }

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

  def load(config: Config, in: ResourceHandle): LogicalLines = {
    val s = in.asText
    parse(config, s)
  }

  def load(config: Config, in: InputSource): LogicalLines = {
    val s = in.asText
    parse(config, s)
  }

  case class Config(
    i18nContext: I18NContext = I18NContext.default,
    useDoubleQuote: Boolean = false,
    useSingleQuote: Boolean = false,
    useAngleBracket: Boolean = false, // XML
    useBrace: Boolean = false, // JSON
    useParenthesis: Boolean = false, // S-Expression (Lisp)
    useBracket: Boolean = false,
    useMultiline: Boolean = false, // SmartDox
    useList: Boolean = false,
    useVerbatim: Boolean = false,
    verbatims: Vector[LogicalBlock.VerbatimMarkClass] = Vector.empty,
    isLocation: Boolean = true
  ) extends ParseConfig {
    def withoutLocation = copy(isLocation = false)
    def addVerbatims(p: Seq[LogicalBlock.VerbatimMarkClass]) = copy(
      verbatims = verbatims ++ p
    )

    def isWordSeparating(prev: Char, next: Char): Boolean = i18nContext.stringFormatter.isWordSeparating(prev, next)
    def wordSeparatingSpace: Char = ' '
    def isInList(c: Char): Boolean = {
      val candidates = Vector('-')
      candidates.contains(c) || StringUtils.isAsciiNumberChar(c)
    }
    def isInTable(c: Char): Boolean = {
      val candidates = Vector('|', '│', '┃')
      candidates.contains(c)
    }
    def isInListLineStart(p: String): Boolean = p.startsWith("-") // TODO
    def isInVerbatim(p: String): Boolean = verbatims.exists(_.isMatch(p))
    def getVerbatimMark(p: String): Option[LogicalBlock.VerbatimMark] = verbatims.toStream.flatMap(_.get(p)).headOption
  }
  object Config {
    val raw = Config()
//    val script = Config(I18NContext.default, true, true, true, true, true, true, true, true, true)
    val script = Config(I18NContext.default, true, true, true, true, true, true, false, true, false)
    val lisp = script.copy(useSingleQuote = false, useList = false)
    val easyText = raw.copy(useMultiline = true, useVerbatim = true, useList = true, verbatims = Vector(LogicalBlock.RawBackquoteMarkClass))
    val easyHtml = easyText.copy(useAngleBracket = true)
    val default = script
    val literateModel = easyHtml // .copy(useMultiline = false)
  }

  trait LogicalLinesParseState extends ParseReaderWriterState[Config, LogicalLines] {
    protected def use_double_quote(config: Config) = config.useDoubleQuote
    protected def use_single_quote(config: Config) = config.useSingleQuote
    protected def use_angle_bracket(config: Config) = config.useAngleBracket
    protected def use_brace(config: Config) = config.useBrace
    protected def use_parenthesis(config: Config) = config.useParenthesis
    protected def use_bracket(config: Config) = config.useBracket
    protected def use_multiline(config: Config) = config.useMultiline
    protected def use_multiline_in_list(config: Config) = use_multiline(config)
    protected def use_verbatim(config: Config) = config.useVerbatim
    protected def is_word_separating(config: Config, prev: Option[Char], next: Char) =
      prev.map(config.isWordSeparating(_, next)).getOrElse(false)
    protected def word_separating_space(config: Config) = config.wordSeparatingSpace
    protected def is_in_list(config: Config, c: Char) = config.isInList(c)
    // protected def is_in_table(config: Config, c: Char) =config.isInTable(c)
    // protected def is_in_list_line_start(config: Config, line: Option[String]) =
    //   line.fold(false)(config.isInListLineStart)
    // protected def is_in_list_line_start(config: Config, evt: CharEvent) = evt.c == '-'
    protected def is_in_table(config: Config, c: Char) = config.isInTable(c)
    protected def is_in_list_line_start(config: Config, line: Option[String]) =
      config.useList && line.fold(false)(config.isInListLineStart)
    protected def is_in_list_line_start(config: Config, cs: Seq[Char], evt: CharEvent) =
      config.useList && cs.forall(x => x == ' ' || x == '\t') && config.isInList(evt.c)

    protected def is_in_verbatim(config: Config, line: Option[String]) =
      use_verbatim(config) && line.fold(false)(config.isInVerbatim)

    def location: Option[ParseLocation]

    // def getFirstChar: Option[Char]
    def getLastChar: Option[Char]
    def isEmpty: Boolean = getLastChar.isEmpty

    protected def get_Current_Line: Option[String]

    def apply(config: Config, evt: ParseEvent): Transition = {
      // println(s"in($this): $evt")
      val r = handle_event(config, evt)
      // println(s"out($this): $r")
      r
    }

    def addChild(config: Config, ps: Vector[Char]): LogicalLinesParseState = RAISE.noReachDefect(s"addChild(${getClass.getSimpleName})")
    def addChildEndTransition(config: Config, ps: Vector[Char]): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")
    def addChildEndTransition(
      config: Config,
      msgs: ParseMessageSequence,
      ps: Vector[Char]
    ): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")
    def addChildEndTransition(config: Config, l: LogicalLines): Transition =
      RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")

    // def addMultiLine(config: Config, p: String): LogicalLinesParseState =
    //   RAISE.noReachDefect(s"addLine(${getClass.getSimpleName})")

    protected final def handle_event(config: Config, evt: ParseEvent): Transition =
      evt match {
        case StartEvent => handle_start(config)
        case EndEvent => handle_end(config)
        case m: LineEndEvent => handle_line_end(config, m)
        case m: CharEvent => m.c match {
          case '"' if use_double_quote(config) => handle_double_quote(config, m)
          case '\'' if use_single_quote(config) => handle_single_quote(config, m)
          case '<' if use_angle_bracket(config) => handle_open_angle_bracket(config, m)
          case '>' if use_angle_bracket(config) => handle_close_angle_bracket(config, m)
          case '{' if use_brace(config) => handle_open_brace(config, m)
          case '}' if use_brace(config) => handle_close_brace(config, m)
          case '(' if use_parenthesis(config) => handle_open_parenthesis(config, m)
          case ')' if use_parenthesis(config) => handle_close_parenthesis(config, m)
          case '[' if use_bracket(config) => handle_open_bracket(config, m)
          case ']' if use_bracket(config) => handle_close_bracket(config, m)
          case '\n' if is_in_list_line_start(config, get_Current_Line) => handle_newline_list(config, m)
          case '\n' if is_in_verbatim(config, get_Current_Line) => handle_newline_verbatim(config, m)
          case '\n' if use_multiline(config) => handle_newline_multiline(config, m)
          case '\n' => handle_newline(config, m)
          case '\r' if is_in_list_line_start(config, get_Current_Line) => handle_carrige_return_list(config, m)
          case '\r' if is_in_verbatim(config, get_Current_Line) => handle_carrige_return_verbatim(config, m)
          case '\r' if use_multiline(config) => handle_carrige_return_multiline(config, m)
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

    protected def physical_Line_End_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      RAISE.notImplementedYetDefect(this, "line_End_State")

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

    protected final def handle_newline_multiline(config: Config, evt: CharEvent): Transition =
      handle_Newline_Multiline(config, evt)

    protected def handle_Newline_Multiline(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, newline_State_Multiline(config, evt))

    protected def newline_State_Multiline(config: Config, evt: CharEvent): LogicalLinesParseState =
      if (isEmpty)
        line_End_State(config, LineEndEvent(evt.location))
      else
        _newline_state_multiline(config, evt)

    private def _newline_state_multiline(config: Config, evt: CharEvent): LogicalLinesParseState = {
      def _not_use_multiline_(c: Char) = (!use_multiline_in_list(config) && is_in_list(config, c)) || is_in_table(config, c)
      def _is_ward_separating_(c: Char) = is_word_separating(config, getLastChar, c) && !is_in_list(config, c)
      // evt.next match {
      //   case Some('\n') => line_End_State(config, LineEndEvent(evt.location))
      //   case Some('\r') => line_End_State(config, LineEndEvent(evt.location))
      //   case Some(c) if _not_use_multiline_(c) => line_End_State(config, LineEndEvent(evt.location))
      //   case Some(c) if _is_ward_separating_(c) => character_State(word_separating_space(config))

      def _is_heavy_title_(c: Char) =
        if (_is_heavy_title_char_(c)) {
          val mark = c
          evt.next2 match {
            case Some(c2) =>
              if (c2 == c)
                evt.next3 match {
                  case Some(c3) => c3 == c || c3 == '\n' || c3 == '\r'
                  case None => true
                }
              else
                c2 == '\n' || c2 == '\r'
            case None => true
          }
        } else {
          false
        }

      def _is_heavy_title_char_(c: Char) = c match {
        case '=' => true
        case '-' => true
        case _ => false
      }

      evt.next match {
        case Some('\n') => line_End_State(config, LineEndEvent(evt.location))
        case Some('\r') => line_End_State(config, LineEndEvent(evt.location)) // TODO CRLF
        case Some(c) if _not_use_multiline_(c) => line_End_State(config, LineEndEvent(evt.location))
//        case Some(c) if _use_heavy_title_(c) => HeavyTitleCandidateState(this, NonEmptyVector(c), Some(evt.location))
        case Some(c) if _is_heavy_title_(c) => physical_Line_End_State(config, evt)
        case Some(c) if _is_ward_separating_(c) => physical_Line_End_State(config, evt) // character_State(word_separating_space(config))
        case Some(c) => this
        case None => line_End_State(config, LineEndEvent(evt.location))
      }
    }

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

    protected final def handle_carrige_return_multiline(config: Config, evt: CharEvent): Transition =
      handle_Carrige_Return_Multiline(config, evt)

    protected def handle_Carrige_Return_Multiline(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, carrige_Return_State_Mutiline(config, evt))

    protected def carrige_Return_State_Mutiline(config: Config, evt: CharEvent): LogicalLinesParseState =
      evt.next match {
        case Some('\n') => SkipState(line_End_State(config, LineEndEvent(evt.location)))
        case Some('\r') => line_End_State(config, LineEndEvent(evt.location))
        case Some(c) if is_word_separating(config, getLastChar, c) => character_State(word_separating_space(config))
        case Some(c) => this
        case None => line_End_State(config, LineEndEvent(evt.location))
      }
      // evt.next match {
      //   case Some('\n') => SkipState(
      //     line_End_State(config, LineEndEvent(evt.location))
      //   )
      //   case _ => line_End_State(config, LineEndEvent(evt.location))
      // }

    protected final def handle_newline_list(config: Config, evt: CharEvent): Transition =
      handle_Newline_List(config, evt)

    protected def handle_Newline_List(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, newline_List_State(config, evt))

    protected def newline_List_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      newline_List_State(evt.c)

    protected def newline_List_State(c: Char): LogicalLinesParseState = {
      RAISE.notImplementedYetDefect(this, "newline_List_State")
      // try {
      //   RAISE.notImplementedYetDefect(this, "newline_List_State")
      // } catch {
      //   case e =>
      //     println(s"LogicalLines: $this")
      //     e.printStackTrace; throw e
      // }
    }

    protected final def handle_carrige_return_list(config: Config, evt: CharEvent): Transition =
      handle_Carrige_Return_List(config, evt)

    protected def handle_Carrige_Return_List(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, carrige_Return_List_State(config, evt))

    protected def carrige_Return_List_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      carrige_Return_List_State(evt.c)

    protected def carrige_Return_List_State(c: Char): LogicalLinesParseState =
      RAISE.notImplementedYetDefect(this, "carrige_Return_List_State")

    protected final def handle_newline_verbatim(config: Config, evt: CharEvent): Transition =
      handle_Newline_Verbatim(config, evt)

    protected def handle_Newline_Verbatim(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, newline_Verbatim_State(config, evt))

    protected def newline_Verbatim_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      newline_Verbatim_State(evt.c)

    protected def newline_Verbatim_State(c: Char): LogicalLinesParseState =
      RAISE.notImplementedYetDefect(this, "newline_Verbatim_State")

    protected final def handle_carrige_return_verbatim(config: Config, evt: CharEvent): Transition =
      handle_Carrige_Return_Verbatim(config, evt)

    protected def handle_Carrige_Return_Verbatim(config: Config, evt: CharEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, carrige_Return_Verbatim_State(config, evt))

    protected def carrige_Return_Verbatim_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      carrige_Return_Verbatim_State(evt.c)

    protected def carrige_Return_Verbatim_State(c: Char): LogicalLinesParseState =
      RAISE.notImplementedYetDefect(this, "carrige_Return_Verbatim_State")

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

  // Legacy : Use AdvancedAwakeningLogicalLinesParseState
  sealed trait AwakeningLogicalLinesParseState extends LogicalLinesParseState {
    override protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)

    override protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      character_State(config, evt)
  }

  sealed trait AdvancedAwakeningLogicalLinesParseState extends AwakeningLogicalLinesParseState {
    override protected def use_single_quote(config: Config) = false
    override protected def use_angle_bracket(config: Config) = false
    override protected def use_brace(config: Config) = false
    override protected def use_parenthesis(config: Config) = false
    override protected def use_bracket(config: Config) = false
    override protected def use_multiline(config: Config) = false
  }

  // sealed trait LineEndLogicalLinesParseState extends LogicalLinesParseState {
  //   override protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
  //     lineend_State(config, evt)

  //   override protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
  //     evt.next match {
  //       case Some(c) => c match {
  //         case '\n' => SkipState(this)
  //         case _ => lineend_State(config, evt)
  //       }
  //       case None => lineend_State(config, evt)
  //     }

  //   protected def lineend_State(config: Config, evt: CharEvent): LogicalLinesParseState =
  //     lineend_State(evt.c)

  //   protected def lineend_State(c: Char): LogicalLinesParseState =
  //     RAISE.notImplementedYetDefect(this, "lineend_State")
  // }

  case object InitState extends LogicalLinesParseState {
    val location = None
    def getLastChar = None
    protected def get_Current_Line = None
    override protected def end_Result(config: Config) = ParseSuccess(LogicalLines.empty)
    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState =
      NormalState("", evt.location)
    override protected def character_State(config: Config, evt: CharEvent) =
      if (is_in_list_line_start(config, "", evt))
        ListState(NormalState("", evt.location), evt.c, Some(evt.location))
      else
        NormalState(evt.c, evt.location)
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
    override protected def open_Bracket_State(config: Config, evt: CharEvent) = BracketState(NormalState(evt.location), evt)
    // override protected def close_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect
  }

  case object EndState extends LogicalLinesParseState {
    val location = None
    def getLastChar = None
    protected def get_Current_Line = None
    override def apply(config: Config, evt: ParseEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, this)
  }

  case class SkipState(next: Transition) extends LogicalLinesParseState {
    val location = None
    def getLastChar = None
    protected def get_Current_Line = None
    override def apply(config: Config, evt: ParseEvent): Transition = next
  }
  object SkipState {
    def apply(p: LogicalLinesParseState): SkipState = SkipState(
      ParseMessageSequence.empty, ParseResult.empty, p
    )
  }

  case class NormalState(
    cs: Slot,
    location: Option[ParseLocation],
    result: LogicalLines
  ) extends LogicalLinesParseState {
    def getLastChar = cs.lastOption
    protected def get_Current_Line = Some(cs.mkString)
    override def addChild(config: Config, ps: Vector[Char]) = copy(cs = cs ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(cs = cs ++ ps)(config, EndEvent)

    override def addChildEndTransition(config: Config, p: LogicalLines): Transition = {
      val a = _make_result(config)
      val r = a + p
      (ParseMessageSequence.empty, ParseSuccess(r), end_State(config))
    }

    // TODO rename
    // override def addMultiLine(config: Config, p: String) = copy(cs = cs.addMultiLine(p))

    def addLines(p: LogicalLines) = copy(result = result + p)

    override protected def end_Result(config: Config) = {
      val r = _make_result(config)
      ParseSuccess(r)
    }

    private def _make_result(config: Config) =
      if (cs.isEmpty)
        result
      else
        result :+ cs.toLogicalLine(get_location(config))

    override protected def handle_Newline(config: Config, evt: CharEvent): Transition =
      handle_Line_End(config, LineEndEvent(evt.location))

    override protected def handle_Line_End(config: Config, evt: LineEndEvent): Transition = {
      val r = result :+ cs.toLogicalLine(get_location(config))
      (ParseMessageSequence.empty, ParseSuccess(r), NormalState.empty)
    }

    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState =
      NormalState(Slot.empty, Some(evt.location), result :+ cs.toLogicalLine(get_location(config)))

    override protected def physical_Line_End_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      copy(cs = cs.addPhysicalLineEnd(word_separating_space(config)), location = Some(evt.location))

    override protected def character_State(config: Config, evt: CharEvent) =
      if (is_in_list_line_start(config, cs.cs, evt))
        ListState(this, evt.c, Some(evt.location))
      else if (location.isDefined)
        character_State(evt.c)
      else
        copy(cs = cs :+ evt.c, location = Some(evt.location))

    override protected def character_State(c: Char): LogicalLinesParseState =
      copy(cs = cs :+ c)
    override protected def double_Quote_State(config: Config, evt: CharEvent) = DoubleQuoteState(this, evt.location)
    override protected def single_Quote_State(config: Config, evt: CharEvent) = SingleQuoteState(this, evt.location)
    override protected def open_Angle_Bracket_State(config: Config, evt: CharEvent) = XmlOpenState(this, Some(evt.location))
//    override protected def close_Angle_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Angle_Bracket_State")
    override protected def open_Brace_State(config: Config, evt: CharEvent) = JsonState(this, evt.location)
//    override protected def close_Brace_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Brace_State")
    override protected def open_Parenthesis_State(config: Config, evt: CharEvent) = SExpressionState(this, evt.location)
//    override protected def close_Parenthesis_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Parenthesis_State")
    override protected def open_Bracket_State(config: Config, evt: CharEvent) = BracketState(this, evt)
//    override protected def close_Bracket_State(config: Config, evt: CharEvent) = RAISE.notImplementedYetDefect(this, "close_Bracket_State")

    override protected def newline_Verbatim_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      _verbatim_state(config, evt)

    private def _verbatim_state(config: Config, evt: CharEvent): LogicalLinesParseState = {
      val s = cs.mkString
      val mark = config.getVerbatimMark(s).getOrElse(RAISE.noReachDefect)
      val parent = copy(cs = Slot.empty)
      VerbatimState(parent, mark, s, location orElse Some(evt.location))
    }

    override protected def carrige_Return_Verbatim_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      if (evt.next == Some('\n'))
        SkipState(_verbatim_state(config, evt))
      else
        _verbatim_state(config, evt)
  }
  object NormalState {
    val empty = NormalState(Slot.empty, None, LogicalLines.empty)
    def apply(p: Char, location: ParseLocation): NormalState = NormalState(Slot(p), Some(location), LogicalLines.empty)
    def apply(p: String, location: ParseLocation): NormalState = NormalState(Slot.empty, Some(location), LogicalLines(p))
    def apply(location: ParseLocation): NormalState = NormalState(Slot.empty, Some(location), LogicalLines.empty)
  }

  // Unused
  case class HeavyTitleCandidateState(
    parent: LogicalLinesParseState,
    mark: NonEmptyVector[Char],
    location: Option[ParseLocation]
  ) extends LogicalLinesParseState {
    def getLastChar = Some(mark.last)
    protected def get_Current_Line = Some(mark.mkString)

    // override protected def lineend_State(config: Config, evt: CharEvent): LogicalLinesParseState =
    //   parent.addLine(config, mark.vector.mkString)

    override protected def line_End_State(config: Config, evt: LineEndEvent): LogicalLinesParseState =
      // parent.addMultiLine(config, mark.vector.mkString)
      RAISE.noReachDefect(s"line_End_State(${getClass.getSimpleName})")

    override protected def character_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      if (mark.head == evt.c)
        copy(mark = mark :+ evt.c)
      else
        parent.addChild(config, mark.vector :+ evt.c)
  }

  case class SExpressionState(
    parent: LogicalLinesParseState,
    count: Int,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AwakeningLogicalLinesParseState {
    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)
    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(text = text ++ ps)(config, EndEvent)

    override def addChildEndTransition(
      config: Config,
      msgs: ParseMessageSequence,
      ps: Vector[Char]
    ): Transition = {
      val s = (text ++ ps).mkString
      (msgs, _end_result(s), end_State(config))
    }

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      _end_result(text.mkString)

    private def _end_result(p: String): ParseResult[LogicalLines] =
      ParseFailure(s"Parenthesis is not closed: ${p}", location)

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
    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)
    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(text = text ++ ps)(config, EndEvent)

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      ParseFailure(s"Json is not completed: ${text.mkString}", location)

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
  ) extends AdvancedAwakeningLogicalLinesParseState {
    override protected def use_angle_bracket(config: Config) = true

    def getLastChar = tag.lastOption
    protected def get_Current_Line = Some(tag.mkString)
    lazy val tagName = tag.takeWhile(_not_delimiterp).mkString
    lazy val tagString = s"<${tag.mkString}>"

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      ParseFailure(s"Xml is not completed: ${tag.mkString}", location)

    private def _not_delimiterp(p: Char) = !_delimiterp(p)

    private def _delimiterp(p: Char) = p match {
      case ' ' => true
      case _ => false
    }

    override def addChild(config: Config, ps: Vector[Char]) = copy(tag = tag ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(tag = tag ++ ps)(config, EndEvent)

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
  ) extends AdvancedAwakeningLogicalLinesParseState {
    override protected def use_angle_bracket(config: Config) = true

    def getLastChar = tag.lastOption
    protected def get_Current_Line = Some(tag.mkString)
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
  ) extends AdvancedAwakeningLogicalLinesParseState {
    override protected def use_angle_bracket(config: Config) = true

    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)
    lazy val textString = text.mkString

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      ParseFailure(s"Xml is not completed: ${text.mkString}", location)

    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(text = text ++ ps)(config, EndEvent)

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
    number: Int,
    count: Int = 0,
    text: Vector[Char] = Vector.empty
  ) extends AwakeningLogicalLinesParseState {
    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)
    override def addChild(config: Config, ps: Vector[Char]) = copy(text = text ++ ps)
    override def addChildEndTransition(config: Config, ps: Vector[Char]): Transition =
      copy(text = text ++ ps)(config, EndEvent)

    override protected def end_Result(config: Config): ParseResult[LogicalLines] =
      ParseFailure(s"Bracket is not closed: ${text.mkString}", location)

    override protected def character_State(c: Char) = copy(text = text :+ c, count = 0)
    override protected def open_Bracket_State(config: Config, evt: CharEvent) =
      if (text.isEmpty)
        this
      else
        super.open_Brace_State(config, evt)
    override protected def close_Bracket_State(config: Config, evt: CharEvent) = {
      val n = count + 1
      if (number == n) {
        val prefix = Vector.fill(number)('[')
        val postfix = Vector.fill(number)(']')
        parent.addChild(config, prefix ++ text ++ postfix)
      } else {
        copy(count = n)
      }
    }
  }
  object BracketState {
    // def apply(parent: LogicalLinesParseState, location: ParseLocation): BracketState =
    //   BracketState(parent, Some(location))

    def apply(parent: LogicalLinesParseState, evt: CharEvent): BracketState =
      (evt.c, evt.next, evt.next2) match {
        case (c, Some('['), Some('[')) => BracketState(parent, Some(evt.location), 3)
        case (c, Some('['), _) => BracketState(parent, Some(evt.location), 2)
        case _ => BracketState(parent, Some(evt.location), 1)
      }
  }

  case class DoubleQuoteState(
    parent: LogicalLinesParseState,
    location: Option[ParseLocation],
    text: Vector[Char] = Vector.empty,
    isRaw: Boolean = false
  ) extends AdvancedAwakeningLogicalLinesParseState {
    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)

    override protected def handle_End(config: Config): Transition = {
      val cs = '"' +: text :+ '"'
      parent.addChildEndTransition(
        config,
        ParseMessageSequence.warning(location, s"""In '"', reach to unexpected end: ${cs.mkString}"""),
        cs
      )
    }
    override protected def character_State(c: Char) = copy(text = text :+ c)
    override protected def double_Quote_State(config: Config, evt: CharEvent) =
      if (text.isEmpty && isRaw == false && evt.c == '"' && evt.next == Some('"')) {
        SkipState(copy(isRaw = true, text = Vector('"', '"')))
      } else if (isRaw) {
        if (evt.c == '"' && evt.next == Some('"') && evt.next2 == Some('"') && evt.next3 != Some('"'))
          SkipState(SkipState(parent.addChild(config, ('"' +: text) ++ "\"\"\"")))
        else
          copy(text = text :+ '"')
      } else {
        parent.addChild(config, '"' +: text :+ '"')
      }
  }
  object DoubleQuoteState {
    def apply(parent: LogicalLinesParseState, location: ParseLocation): DoubleQuoteState =
      DoubleQuoteState(parent, Some(location))
  }

  case class SingleQuoteState(
    parent: LogicalLinesParseState,
    text: Vector[Char],
    location: Option[ParseLocation]
  ) extends AdvancedAwakeningLogicalLinesParseState {
    def getLastChar = text.lastOption
    protected def get_Current_Line = Some(text.mkString)

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

  case class VerbatimState(
    parent: NormalState,
    mark: LogicalBlock.VerbatimMark,
    lines: LogicalLines.Builder
  ) extends LogicalLinesParseState {
    def location: Option[ParseLocation] = lines.location

    override protected def use_double_quote(config: Config) = false
    override protected def use_angle_bracket(config: Config) = false
    override protected def use_brace(config: Config) = false
    override protected def use_parenthesis(config: Config) = false
    override protected def use_bracket(config: Config) = false
    override protected def use_multiline(config: Config) = false
    override protected def use_verbatim(config: Config) = false
    override protected def is_in_list_line_start(config: Config, line: Option[String]) =
false
    override protected def is_in_list_line_start(config: Config, cs: Seq[Char], evt: CharEvent) = false

    def getLastChar = lines.lastOption
    protected def get_Current_Line = lines.getCurrentLine

    override protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      _line_end(config, evt)

    override protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      _line_end(config, evt)

    private def _line_end(config: Config, evt: CharEvent) = {
      val s = lines.currentLine
      if (mark.isDone(s)) {
        val ll = lines.build()
        parent.addLines(ll)
      } else {
        copy(lines = lines.lineEnd(evt))
      }
    }

    override protected def character_State(config: Config, evt: CharEvent) =
      copy(lines = lines.add(evt))
  }
  object VerbatimState {
    def apply(
      parent: NormalState,
      mark: LogicalBlock.VerbatimMark,
      s: String,
      location: Option[ParseLocation]
    ): VerbatimState = VerbatimState(
      parent,
      mark,
      LogicalLines.Builder(s, location)
    )
  }

  case class ListState(
    parent: NormalState,
    cs: Vector[Char],
    location: Option[ParseLocation],
    result: LogicalLines
  ) extends LogicalLinesParseState {
    override protected def use_double_quote(config: Config) = false
    override protected def use_angle_bracket(config: Config) = false
    override protected def use_brace(config: Config) = false
    override protected def use_parenthesis(config: Config) = false
    override protected def use_bracket(config: Config) = false
    override protected def use_multiline(config: Config) = false
    override protected def use_verbatim(config: Config) = false

    override protected def is_in_list_line_start(config: Config, cline: Option[String]) = false

    def getLastChar = cs.lastOption
    protected def get_Current_Line = Some(cs.mkString)

    override protected def handle_End(config: Config): Transition = {
      val r = _make_result(config)
      parent.addChildEndTransition(config, r)
    }

    private def _make_result(config: Config) =
      if (cs.isEmpty)
        result
      else
        result :+ LogicalLine(cs.mkString, get_location(config))

    // override protected def end_Result(config: Config) = 
    //   ParseSuccess(LogicalLines.empty)

    // override protected def end_State(config: Config) = {
    //   val r = if (cs.isEmpty)
    //     result
    //   else
    //     result :+ LogicalLine(cs.mkString, get_location(config))
    //   parent.addLines(r)
    // }

    override protected def newline_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      _line_end(config, evt)

    override protected def carrige_Return_State(config: Config, evt: CharEvent): LogicalLinesParseState =
      _line_end(config, evt)

    private def _line_end(config: Config, evt: CharEvent) =
      if (cs.isEmpty)
        parent.addLines(result :+ LogicalLine.empty)
      else
        copy(cs = Vector.empty, result = result :+ cs.mkString)

    override protected def character_State(config: Config, evt: CharEvent) =
      copy(cs = cs :+ evt.c)
  }
  object ListState {
    def apply(
      parent: NormalState,
      c: Char,
      location: Option[ParseLocation]
    ): ListState = ListState(
      parent,
      Vector(c),
      location,
      LogicalLines.empty
    )
  }
}
