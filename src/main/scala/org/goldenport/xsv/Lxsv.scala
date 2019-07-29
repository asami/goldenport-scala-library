package org.goldenport.xsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.Strings
import org.goldenport.parser.{LogicalToken, LogicalTokens}
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}
import org.goldenport.collection.VectorMap
import org.goldenport.extension.IRecord

/*
 * @since   Jul. 16, 2019
 * @version Jul. 26, 2019
 * @author  ASAMI, Tomoharu
 */
case class Lxsv(
  strategy: Xsv.Strategy,
  vectormap: VectorMap[Symbol, LogicalToken]
) extends IRecord {
  lazy val keys: List[Symbol] = vectormap.keys.toList
  lazy val keyNames: List[String] = keys.map(_.name)
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
}

object Lxsv {
  val empty = Lxsv(Xsv.XsvStrategy, VectorMap.empty)

  private val _lxsv_chars = Vector('\t', ';', ',', ' ')
  private val _lxsv_token_chars = Vector('\t', ';', ',')
  private val _complex_chars = Vector('<', '{', '[')

  def apply(ps: Seq[(Symbol, Any)]): Lxsv = {
    val config = LogicalTokens.Config.xsv
    val strategy = Xsv.XsvStrategy
    val xs = ps.map {
      case (k, v) => k -> LogicalToken.create(config, v)
    }
    Lxsv(strategy, VectorMap(xs))
  }

  def create(p: String): Lxsv = parse(p) match {
    case ParseSuccess(s, _) => s
    case m: EmptyParseResult[_] => empty
    case m: ParseFailure[_] => m.RAISE
  }

  def parse(p: String): ParseResult[Lxsv] =
    if (p.contains(':'))
      _parse(p)
    else
      _error(p)

  private def _parse(p: String): ParseResult[Lxsv] =
    _lxsv_chars.toStream.flatMap(x => _parse_option(x, p)).headOption.
      map(ParseResult.success).
      getOrElse(ParseResult.error(s"No lxsv: $p"))

  private def _parse_option(c: Char, p: String): Option[Lxsv] = {
    val xs = Strings.totokens(p, c.toString)
    case class Z(
      keyvalues: Vector[(String, String)] = Vector.empty,
      isError: Boolean = false
    ) {
      def r = if (isError)
        None
      else
        Some(
          Lxsv(keyvalues.map {
            case (k, v) => Symbol(k) -> v
          })
        )

      def +(rhs: String) =
        if (isError)
          this
        else
          Strings.tokeyvalue(rhs) match {
            case (k, "") => Z(isError = true)
            case (k, v) => Z(keyvalues = keyvalues :+ (k -> v))
          }
    }
    xs./:(Z())(_+_).r
  }

  def parseToken(p: String): ParseResult[Lxsv] =
    _lxsv_token_chars.toStream.flatMap(x => _parse_option(x, p)).headOption.
      map(ParseResult.success).
      getOrElse(ParseResult.error(s"No lxsv: $p"))

  private def _error(p: String): ParseResult[Lxsv] = ParseResult.error(p)

  def isLxsv(p: String): Boolean =
    p.contains(':') && _is_lxsv(_lxsv_chars, p)

  def isLxsvToken(p: String): Boolean =
    p.contains(':') && _is_lxsv(_lxsv_token_chars, p)

  private def _is_lxsv(delimiters: Vector[Char], p: String): Boolean =
    if (_is_complex(p))
      _is_lxsv_complex(delimiters, p)
    else
      _is_lxsv_simple(delimiters, p)

  private def _is_complex(p: String) = p.contains(_complex_chars)

  private def _is_lxsv_simple(delimiters: Vector[Char], p: String): Boolean =
    delimiters.exists(_is_lxsv_simple(_, p))

  private def _is_lxsv_complex(delimiters: Vector[Char], p: String): Boolean =
    delimiters.exists(_is_lxsv_complex(_, p))

  private def _is_lxsv_simple(delimiter: Char, p: String): Boolean = {
    val xs = Strings.totokens(p, delimiter.toString)
    xs.length match {
      case 0 => false
      case 1 => false
      case _ => xs.forall(_.contains(':'))
    }
  }

  private def _is_lxsv_complex(delimiter: Char, p: String): Boolean =
    _parse_option(delimiter, p).map(_.length > 1).getOrElse(false)
}
