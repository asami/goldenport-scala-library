package org.goldenport.xsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.io.{ResourceHandle, InputSource}
import org.goldenport.value._
import org.goldenport.values.NumberRange
import org.goldenport.parser._

/*
 * @since   Jul. 16, 2019
 *  version Aug. 24, 2019
 *  version Sep. 16, 2019
 *  version Nov. 10, 2019
 * @version Dec.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class Xsv(
  strategy: Xsv.Strategy,
  matrix: VectorRowColumnMatrix[LogicalToken]
) extends IMatrix[LogicalToken] {
  def apply(x: Int, y: Int): LogicalToken = matrix.apply(x, y)
  def width: Int = matrix.width
  def height: Int = matrix.height
  override def rowIterator: Iterator[Vector[LogicalToken]] = matrix.rowIterator
  override def columnIterator: Iterator[Vector[LogicalToken]] = matrix.columnIterator
  def projection(p: NumberRange): IMatrix[LogicalToken] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[LogicalToken] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[LogicalToken]): Xsv = copy(matrix = matrix appendRow ps)
  def appendRows(ps: IMatrix[LogicalToken]): Xsv = copy(matrix = matrix appendRows ps)
  def appendColumn(ps: Seq[LogicalToken]): IMatrix[LogicalToken] = RAISE.unsupportedOperationFault
  def appendColumns(ps: IMatrix[LogicalToken]): IMatrix[LogicalToken] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[LogicalToken] = RAISE.notImplementedYetDefect
}

object Xsv {
  sealed trait Strategy extends NamedValueInstance {
    def logicalTokenConfig: LogicalTokens.Config
    def outputKeyValueSeparator: Char
    def outputDelimiter: Char
    def inputDelimiters: Seq[Char]
    def isDelimiter(p: String): Boolean = p.length match {
      case 1 => inputDelimiters.contains(p(0))
      case _ => false
    }
  }
  object Strategy extends EnumerationClass[Strategy] {
    val elements = Vector(XsvStrategy, CsvStrategy, TsvStrategy, SCsvStrategy, SsvStrategy)
  }

  object XsvStrategy extends Strategy {
    val name = "xsv"
    def logicalTokenConfig = LogicalTokens.Config.xsv
    def outputKeyValueSeparator: Char = ':'
    def outputDelimiter: Char = '\t'
    def inputDelimiters: Seq[Char] = ";,\t "
  }
  object CsvStrategy extends Strategy {
    val name = "csv"
    def logicalTokenConfig = LogicalTokens.Config.csv
    def outputKeyValueSeparator: Char = ':'
    def outputDelimiter: Char = ','
    def inputDelimiters: Seq[Char] = ","
  }
  object TsvStrategy extends Strategy {
    val name = "tsv"
    def logicalTokenConfig = LogicalTokens.Config.tsv
    def outputKeyValueSeparator: Char = ':'
    def outputDelimiter: Char = '\t'
    def inputDelimiters: Seq[Char] = "\t"
  }
  object SCsvStrategy extends Strategy {
    val name = "scsv"
    def logicalTokenConfig = LogicalTokens.Config.scsv
    def outputKeyValueSeparator: Char = ':'
    def outputDelimiter: Char = ';'
    def inputDelimiters: Seq[Char] = ";"
  }
  object SsvStrategy extends Strategy {
    val name = "ssv"
    def logicalTokenConfig = LogicalTokens.Config.ssv
    def outputKeyValueSeparator: Char = ':'
    def outputDelimiter: Char = ' '
    def inputDelimiters: Seq[Char] = " \t"
  }
  object UrlStrategy extends Strategy {
    val name = "url"
    def logicalTokenConfig = LogicalTokens.Config.urlEncoding
    def outputKeyValueSeparator: Char = '='
    def outputDelimiter: Char = '&'
    def inputDelimiters: Seq[Char] = "&"
  }

  def parse(p: LogicalParagraph): Xsv = parse(p.lines)
  def parse(p: LogicalLines): Xsv = _parse(p.lines)
  private def _parse(ps: Seq[LogicalLine]): Xsv = parse(ps.map(_.text))
  def parse(ps: Seq[String]): Xsv = parse(XsvStrategy, ps)
  def parseCsv(ps: Seq[String]): Xsv = parse(CsvStrategy, ps)
  def parseTsv(ps: Seq[String]): Xsv = parse(TsvStrategy, ps)
  def parseSCsv(ps: Seq[String]): Xsv = parse(SCsvStrategy, ps)
  def parseSsv(ps: Seq[String]): Xsv = parse(SsvStrategy, ps)

  def parse(s: Strategy, ps: Seq[String]): Xsv = {
    val xs = ps./:(VectorRowColumnMatrix.empty[LogicalToken])((z, x) => z.appendRow(_parse(s, x)))
    Xsv(s, xs)
  }

  private def _parse(s: Strategy, p: String) = {
    val xs = LogicalTokens.parse(s.logicalTokenConfig, p).tokens
    case class Z(
      cold: Vector[LogicalToken] = Vector.empty,
      hot: Vector[LogicalToken] = Vector.empty
    ) {
      def r = if (hot.isEmpty)
        cold
      else
        cold :+ _resolve(hot)

      def +(rhs: LogicalToken) = rhs match {
        case m: DelimiterToken =>
          if (s.isDelimiter(m.s))
            _add_cold()
          else
            _add_hot(m)
        case m => _add_hot(m)
      }

      private def _add_hot(p: LogicalToken) = copy(hot = hot :+ p)

      private def _add_cold() = copy(cold :+ _resolve(hot), Vector.empty)

      private def _resolve(ps: Vector[LogicalToken]): LogicalToken = {
        ps.length match {
          case 0 => EmptyToken
          case 1 => ps.head
          case _ =>
            val sb = ps./:(new StringBuilder)((z, x) => z.append(x.raw))
            SingleStringToken(sb.toString)
        }
      }
    }
    xs./:(Z())(_+_).r
  }

  def load(s: Strategy, in: ResourceHandle): Xsv = {
    val c = LogicalLines.Config.raw // TODO charset
    val ll = LogicalLines.load(c, in)
    parse(ll)
  }

  def load(s: Strategy, in: InputSource): Xsv = {
    val c = LogicalLines.Config.raw // TODO charset
    val ll = LogicalLines.load(c, in)
    parse(ll)
  }
}
