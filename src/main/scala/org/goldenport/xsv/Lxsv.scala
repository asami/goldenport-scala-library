package org.goldenport.xsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.io.{ResourceHandle, InputSource}
import org.goldenport.collection.VectorMap
import org.goldenport.extension.IRecord
import org.goldenport.parser.{LogicalToken, LogicalTokens, StringToken, LogicalLines}
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}

/*
 * @since   Jul. 16, 2019
 *  version Sep. 23, 2019
 *  version Oct. 28, 2019
 *  version Nov. 15, 2019
 *  version Dec.  8, 2019
 * @version Feb. 29, 2020
 * @author  ASAMI, Tomoharu
 */
case class Lxsv(
  strategy: Xsv.Strategy,
  vectormap: VectorMap[Symbol, LogicalToken]
) extends IRecord {
  lazy val keySymbols: List[Symbol] = vectormap.keys.toList
  lazy val keyNames: List[String] = keySymbols.map(_.name)
  def length: Int = vectormap.size
  def get(key: Symbol): Option[Any] = getToken(key)
  def get(key: String): Option[Any] = getToken(key)
  def getToken(key: Symbol): Option[LogicalToken] = vectormap.get(key)
  def getToken(key: String): Option[LogicalToken] = vectormap.get(Symbol(key))
  lazy val print: String = vectormap.map {
    case (k, v) => s"${k.name}${strategy.outputKeyValueSeparator}${v.print}"
  }.mkString(strategy.outputDelimiter.toString)
  def display: String = print // TODO
  def show: String = print // TODO
  def embed: String = print

  def valueMap: Map[Symbol, Any] = vectormap.mapValues(_.value)

  def marshall: String = RAISE.notImplementedYetDefect("Implement simplified format.")

  def withoutLocation: Lxsv = copy(vectormap = vectormap.mapValues(_.clearLocation))
}

object Lxsv {
  val empty = new Lxsv(Xsv.XsvStrategy, VectorMap.empty)

  private val _explicit_key_value_separator_chars = Vector('=')
  private val _implicit_key_value_separator_chars = Vector(':')
  private val _key_value_separator_chars = _explicit_key_value_separator_chars ++ _implicit_key_value_separator_chars
  private val _lxsv_chars = Vector('\t', ';', ',', ' ')
  private val _lxsv_token_chars = Vector('\t', ';', ',')
  private val _lxsv_candidate_chars = _lxsv_token_chars :+ '"'
  private val _complex_chars = Vector('<', '{', '[')

  def apply(strategy: Xsv.Strategy, p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv =
    apply(strategy, p +: ps)

  def apply(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = apply(p +: ps)

  def apply(ps: Seq[(Symbol, Any)]): Lxsv = apply(Xsv.XsvStrategy, ps)

  def apply(strategy: Xsv.Strategy, ps: Seq[(Symbol, Any)]): Lxsv = {
    val config = LogicalTokens.Config.xsv
    val xs = ps.map {
      case (k, v) => k -> LogicalToken.makeToken(config, v)
    }
    Lxsv(strategy, VectorMap(xs))
  }

  def create(p: String): Lxsv = parse(p) match {
    case ParseSuccess(s, _) => s
    case m: EmptyParseResult[_] => empty
    case m: ParseFailure[_] => m.RAISE
  }

  def createOption(p: String): Option[Lxsv] = parse(p) match {
    case ParseSuccess(s, _) => Some(s)
    case m: EmptyParseResult[_] => None
    case m: ParseFailure[_] => None
  }

  def parse(p: String): ParseResult[Lxsv] =
    Parser(_key_value_separator_chars, _lxsv_chars, _complex_chars).apply(p)

  def parseToken(p: String): ParseResult[Lxsv] =
    Parser(_key_value_separator_chars, _lxsv_token_chars, _complex_chars).apply(p)

  def isExplicitLxsvToken(p: String): Boolean =
    p.exists(_explicit_key_value_separator_chars.contains) || (
      p.exists(_implicit_key_value_separator_chars.contains) && p.exists(_lxsv_candidate_chars.contains)
    ) && (
      if (p.contains('"'))
        createOption(p).isDefined
      else
        true
    )

  def load(strategy: Xsv.Strategy, in: ResourceHandle): Vector[Lxsv] = {
    val c = LogicalLines.Config.raw // TODO charset
    val ll = LogicalLines.load(c, in)
    ll.lines.map(x => create(x.text))
  }

  def load(strategy: Xsv.Strategy, in: InputSource): Vector[Lxsv] = {
    val c = LogicalLines.Config.raw // TODO charset
    val ll = LogicalLines.load(c, in)
    ll.lines.map(x => create(x.text))
  }

  case class Parser(
    keyValueSeparatorCandidates: Vector[Char],
    delimiterCandidates: Vector[Char],
    complexChars: Vector[Char]
  ) {
    import Parser._

    private val _config = LogicalTokens.Config.default

    private var _key_value_separator: Char = 0
    private var _delimiter: Char = 0
    private var _state: Int = NORMAL
    private var _psm: LogicalTokens.PartialParserStateMachine = null
    private var _buffer = new StringBuilder()
    private var _key: Symbol = null
    private var _kvs: Vector[(Symbol, LogicalToken)] = Vector.empty
    private var _arg_count: Int = 1 // or 0 ?
    private var _is_complex: Boolean = false

    def apply(p: CharSequence): ParseResult[Lxsv] = {
      for (i <- 0 until p.length) {
        _parse(p.charAt(i))
        if (_is_complex)
          return parseComplex(_key_value_separator, _delimiter, p)
      }
      if (_key != null || !_buffer.isEmpty)
        _flush
      ParseResult.success(Lxsv(_strategy, _kvs))
    }

    private def _parse(c: Char) {
      _state match {
        case NORMAL => c match {
//          case ':' => _flush_key
          case '"' => _state = STRING_CANDIDATE
          case '\'' => _state = SINGLE_QUOTE
          case '{' => _start_partial(c)
          case '<' => _start_partial(c)
          case _ => if (_is_delimiter(c))
            _flush
          else if (_is_key_value_separator(c))
            _flush_key
          else if (_is_complex(c))
            _is_complex = true
          else
            _buffer.append(c)
        }
        case DOUBLE_QUOTE => c match {
          case '"' => _state = NORMAL
          case '\\' => _state = ESCAPE_IN_STRING
          case _ => _buffer.append(c)
        }
        case SINGLE_QUOTE => c match {
          case '\'' => _flush
          case _ => _buffer.append(c)
        }
        case RAW_STRING => c match {
          case '"' => _state = RAW_STRING_CLOSE_CANDIDATE
          case _ => _buffer.append(c)
        }
        case STRING_CANDIDATE => c match {
          case '"' => _state = RAW_STRING_CANDIDATE
          case '\\' => _state = ESCAPE_IN_STRING
          case _ =>
            _buffer.append(c)
            _state = DOUBLE_QUOTE
        }
        case ESCAPE_IN_STRING =>
          c match {
            case '"' => _buffer.append(c)
            case 'n' => _buffer.append('\n')
            case 'r' => _buffer.append('\r')
            case 't' => _buffer.append('\t')
            case 'f' => _buffer.append('\f')
            case _ => _buffer.append(c)
          }
          _state = DOUBLE_QUOTE
        case RAW_STRING_CANDIDATE => c match {
          case '"' => _state = RAW_STRING
          case _ => 
            _buffer.append('"')
            _buffer.append('"')
            _buffer.append(c)
            _state = NORMAL
        }
        case RAW_STRING_CLOSE_CANDIDATE => c match {
          case '"' => _state = RAW_STRING_CLOSE_CANDIDATE2
          case _ => 
            _buffer.append('"')
            _buffer.append(c)
        }
        case RAW_STRING_CLOSE_CANDIDATE2 => c match {
          case '"' => _flush
          case _ =>
            _buffer.append('"')
            _buffer.append('"')
            _buffer.append(c)
        }
        case PARTIAL => _psm.apply(c) match {
          case \/-(t) =>
            _flush_token(t)
            _state = NORMAL
            _psm = null
          case -\/(sm) => _psm = sm
        }
      }
    }

    private def _is_key_value_separator(c: Char) =
      if (_key_value_separator == 0) {
        if (keyValueSeparatorCandidates.contains(c)) {
          _key_value_separator = c
          true
        } else {
          false
        }
      } else {
        _key_value_separator == c
      }

    private def _is_delimiter(c: Char) =
      _key_value_separator match {
        case 0 => _is_delimiter_no_separator(c)
        case '=' => _is_delimiter_in_equal(c)
        case ':' => _is_delimiter_in_colon(c)
      }

    private def _is_delimiter_no_separator(c: Char) = _is_delimiter_in_colon(c)

    private def _is_delimiter_in_equal(c: Char) =
      if (_delimiter == 0) {
        if (c == '&') {
          _delimiter = c
          true
        } else {
          false
        }
      } else {
        _delimiter == c
      }

    private def _is_delimiter_in_colon(c: Char) =
      if (_delimiter == 0) {
        if (delimiterCandidates.contains(c)) {
          _delimiter = c
          true
        } else {
          false
        }
      } else {
        _delimiter == c
      }

    private def _is_complex(c: Char) = complexChars.contains(c)

    private def _flush {
      if (_key == null) {
        _kvs = _kvs :+ (Symbol(s"_${_arg_count}") -> StringToken(_buffer))
        _arg_count += 1
      } else {
        _kvs = _kvs :+ (_key -> StringToken(_buffer))
      }
      _key = null
      _buffer = new StringBuilder
    }

    private def _flush_key {
      _key = Symbol(_buffer.toString)
      _buffer = new StringBuilder
    }

    private def _flush_token(p: LogicalToken) {
      val t =
        if (_buffer.isEmpty)
          p
        else
          StringToken(_buffer ++ p.raw)
      if (_key == null) {
        _kvs = _kvs :+ (Symbol(s"_${_arg_count}") -> t) // TODO buffer
        _arg_count += 1
      } else {
        _kvs = _kvs :+ (_key -> t) 
      }
      _key = null
      _buffer = new StringBuilder
    }

    private def _strategy = _delimiter match {
      case 0 => Xsv.XsvStrategy
      case ',' => Xsv.CsvStrategy
      case '\t' => Xsv.TsvStrategy
      case ';' => Xsv.SCsvStrategy
      case ' ' => Xsv.SsvStrategy
      case '&' => Xsv.UrlStrategy
    }

    private def _start_partial(c: Char): Unit = {
      _state = PARTIAL
      LogicalTokens.PartialParserStateMachine(_config).apply(c) match {
        case \/-(t) => RAISE.noReachDefect
        case -\/(sm) => _psm = sm
      }
    }

    def parseComplex(
      keyvalueseparator: Char,
      delimiter: Char,
      p: CharSequence
    ): ParseResult[Lxsv] = {
      // val xs = Strings.totokens(p, delimiter.toString)
      // case class Z(
      //   keyvalues: Vector[(String, String)] = Vector.empty,
      //   isError: Boolean = false
      // ) {
      //   def r = if (isError)
      //     None
      //   else
      //     Some(
      //       Lxsv(keyvalues.map {
      //         case (k, v) => Symbol(k) -> v
      //       })
      //     )

      //   def +(rhs: String) =
      //     if (isError)
      //       this
      //     else
      //       Strings.tokeyvalue(rhs) match {
      //         case (k, "") => Z(isError = true)
      //         case (k, v) => Z(keyvalues = keyvalues :+ (k -> v))
      //       }
      // }
      // xs./:(Z())(_+_).r
      RAISE.notImplementedYetDefect
    }
  }
  object Parser {
    val NORMAL = 1
    val DOUBLE_QUOTE = 2
    val SINGLE_QUOTE = 3
    val RAW_STRING = 4
    val STRING_CANDIDATE = 5
    val ESCAPE_IN_STRING = 6
    val RAW_STRING_CANDIDATE = 7
    val RAW_STRING_CLOSE_CANDIDATE = 8
    val RAW_STRING_CLOSE_CANDIDATE2 = 9
    val PARTIAL = 10
  }
}
