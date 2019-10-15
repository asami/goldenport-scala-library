package org.goldenport.xsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.parser.{LogicalToken, LogicalTokens}
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}
import org.goldenport.collection.VectorMap
import org.goldenport.extension.IRecord

/*
 * @since   Jul. 16, 2019
 *  version Sep. 23, 2019
 * @version Oct. 12, 2019
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
    case (k, v) => s"${k.name}:${v.print}"
  }.mkString(strategy.outputSeparator.toString)
  def display: String = print // TODO
  def show: String = print // TODO

  def valueMap: Map[Symbol, Any] = vectormap.mapValues(_.value)

//  def withoutLocation: Lxsv = copy(vectormap = vectormap.mapValues(_.clearLocation))
}

object Lxsv {
  val empty = new Lxsv(Xsv.XsvStrategy, VectorMap.empty)

  private val _lxsv_chars = Vector('\t', ';', ',', ' ')
  private val _lxsv_token_chars = Vector('\t', ';', ',')
  private val _complex_chars = Vector('<', '{', '[')

  def apply(strategy: Xsv.Strategy, p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = apply(p +: ps)

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

//  def createWithoutLocation(p: String): Lxsv = create(p).withoutLocation

  def parse(p: String): ParseResult[Lxsv] =
    Parser(_lxsv_chars, _complex_chars).apply(p)

  def parseToken(p: String): ParseResult[Lxsv] =
    Parser(_lxsv_token_chars, _complex_chars).apply(p)

  // def parse(p: String): ParseResult[Lxsv] =
  //   if (p.contains(':'))
  //     _parse(p)
  //   else
  //     _error(p)

  // private def _parse(p: String): ParseResult[Lxsv] =
  //   _lxsv_chars.toStream.flatMap(x => _parse_option(x, p)).headOption.
  //     map(ParseResult.success).
  //     getOrElse(ParseResult.error(s"No lxsv: $p"))

  // private def _parse_option(c: Char, p: String): Option[Lxsv] = {
  //   val xs = Strings.totokens(p, c.toString)
  //   case class Z(
  //     keyvalues: Vector[(String, String)] = Vector.empty,
  //     isError: Boolean = false
  //   ) {
  //     def r = if (isError)
  //       None
  //     else
  //       Some(
  //         Lxsv(keyvalues.map {
  //           case (k, v) => Symbol(k) -> v
  //         })
  //       )

  //     def +(rhs: String) =
  //       if (isError)
  //         this
  //       else
  //         Strings.tokeyvalue(rhs) match {
  //           case (k, "") => Z(isError = true)
  //           case (k, v) => Z(keyvalues = keyvalues :+ (k -> v))
  //         }
  //   }
  //   xs./:(Z())(_+_).r
  // }

  // def parseToken(p: String): ParseResult[Lxsv] =
  //   _lxsv_token_chars.toStream.flatMap(x => _parse_option(x, p)).headOption.
  //     map(ParseResult.success).
  //     getOrElse(ParseResult.error(s"No lxsv: $p"))

  // private def _error(p: String): ParseResult[Lxsv] = ParseResult.error(p)

  // def isExplicitLxsv(p: String): Boolean =
  //   p.contains(':') && _is_lxsv(_lxsv_chars, p)

  def isExplicitLxsvToken(p: String): Boolean =
    p.contains(':') && p.contains(_lxsv_token_chars)

  // private def _is_lxsv(delimiters: Vector[Char], p: String): Boolean =
  //   if (_is_complex(p))
  //     _is_lxsv_complex(delimiters, p)
  //   else
  //     _is_lxsv_simple(delimiters, p)

  // private def _is_complex(p: String) = p.contains(_complex_chars)

  // private def _is_lxsv_simple(delimiters: Vector[Char], p: String): Boolean =
  //   delimiters.exists(_is_lxsv_simple(_, p))

  // private def _is_lxsv_complex(delimiters: Vector[Char], p: String): Boolean =
  //   delimiters.exists(_is_lxsv_complex(_, p))

  // private def _is_lxsv_simple(delimiter: Char, p: String): Boolean = {
  //   val xs = Strings.totokens(p, delimiter.toString)
  //   xs.length match {
  //     case 0 => false
  //     case 1 => false
  //     case _ => xs.forall(_.contains(':'))
  //   }
  // }

  // private def _is_lxsv_complex(delimiter: Char, p: String): Boolean =
  //   _parse_option(delimiter, p).map(_.length > 1).getOrElse(false)

  case class Parser(
    delimiterCandidates: Vector[Char],
    complexChars: Vector[Char]
  ) {
    import Parser._

    private var _delimiter: Char = 0
    private var _state: Int = NORMAL
    private var _buffer = new StringBuilder()
    private var _key: Symbol = null
    private var _kvs: Vector[(Symbol, String)] = Vector.empty
    private var _arg_count: Int = 0
    private var _is_complex: Boolean = false

    def apply(p: CharSequence): ParseResult[Lxsv] = {
      for (i <- 0 until p.length) {
        _parse(p.charAt(i))
        if (_is_complex)
          return parseComplex(_delimiter, p)
      }
      if (_key != null || !_buffer.isEmpty)
        _flush
      ParseResult.success(Lxsv(_strategy, _kvs))
    }

    private def _parse(c: Char) {
      _state match {
        case NORMAL => c match {
          case ':' => _flush_key
          case '"' => _state = STRING_CANDIDATE
          case '\'' => _state = SINGLE_QUOTE
          case _ => if (_is_delimiter(c))
            _flush
          else if (_is_complex(c))
            _is_complex = true
          else
            _buffer.append(c)
        }
        case DOUBLE_QUOTE => c match {
          case '"' => _state = NORMAL
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
          case _ =>
            _buffer.append(c)
            _state = DOUBLE_QUOTE
        }
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
      }
    }

    // private def _parse_before_delimiter(c: Char) {
    //   _state match {
    //     case NORMAL => c match {
    //       case '"' => _state = DOUBLE_QUOTE
    //       case '\'' => _state = SINGLE_QUOTE
    //       case _ =>
    //         if (delimiterCandidates.contains(c))
    //           _delimiter = c
    //         _flush
    //     }
    //     case DOUBLE_QUOTE => c match {
    //       case '"' => _state = RAW_STRING_CANDIDATE
    //       case _ =>
    //     }
    //     case SINGLE_QUOTE => c match {
    //       case '\'' =>
    //         _state = NORMAL
    //       case _ =>
    //     }
    //   }
    // }

    private def _is_delimiter(c: Char) =
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
        _kvs = _kvs :+ (Symbol(s"_${_arg_count}") -> _buffer.toString)
        _arg_count += 1
      } else {
        _kvs = _kvs :+ (_key -> _buffer.toString)
      }
      _buffer = new StringBuilder
    }

    private def _flush_key {
      _key = Symbol(_buffer.toString)
      _buffer = new StringBuilder
    }

    private def _strategy = _delimiter match {
      case 0 => Xsv.XsvStrategy
      case ',' => Xsv.CsvStrategy
      case '\t' => Xsv.TsvStrategy
      case ';' => Xsv.SCsvStrategy
      case ' ' => Xsv.SsvStrategy
    }

    def parseComplex(delimiter: Char, p: CharSequence): ParseResult[Lxsv] = {
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
    val RAW_STRING_CANDIDATE = 6
    val RAW_STRING_CLOSE_CANDIDATE = 7
    val RAW_STRING_CLOSE_CANDIDATE2 = 8
  }
}
