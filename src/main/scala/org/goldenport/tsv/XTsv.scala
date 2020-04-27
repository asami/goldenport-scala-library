package org.goldenport.tsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.matrix._
import org.goldenport.parser.{LogicalParagraph, LogicalLine, XmlOrJsonOrToken}
import org.goldenport.values.NumberRange

/*
 * @since   Jul.  7, 2019
 *  version Aug. 24, 2019
 *  version Sep. 16, 2019
 * @version Feb. 26, 2020
 * @author  ASAMI, Tomoharu
 */
case class XTsv(matrix: VectorRowColumnMatrix[XmlOrJsonOrToken]) extends IMatrix[XmlOrJsonOrToken] {
  def apply(x: Int, y: Int): XmlOrJsonOrToken = matrix.apply(x, y)
  def width: Int = matrix.width
  def height: Int = matrix.height
  override def rowIterator: Iterator[Vector[XmlOrJsonOrToken]] = matrix.rowIterator
  override def columnIterator: Iterator[Vector[XmlOrJsonOrToken]] = matrix.columnIterator
  def select(p: NumberRange): IMatrix[XmlOrJsonOrToken] = RAISE.notImplementedYetDefect
  def filter(p: NumberRange): IMatrix[XmlOrJsonOrToken] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[XmlOrJsonOrToken]): IMatrix[XmlOrJsonOrToken] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[XmlOrJsonOrToken]): IMatrix[XmlOrJsonOrToken] = RAISE.unsupportedOperationFault
  def appendColumn(ps: Seq[XmlOrJsonOrToken]): IMatrix[XmlOrJsonOrToken] = RAISE.unsupportedOperationFault
  def appendColumns(ps: IMatrix[XmlOrJsonOrToken]): IMatrix[XmlOrJsonOrToken] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[XmlOrJsonOrToken] = RAISE.notImplementedYetDefect
}

object XTsv {
  def create(p: LogicalParagraph): XTsv = create(p.lines.lines)

  def create(ps: Vector[LogicalLine]): XTsv =
    XTsv(VectorRowColumnMatrix(ps.map(createRow)))

  def createRow(p: LogicalLine): Vector[XmlOrJsonOrToken] = createRow(p.text)

  def createRow(p: String): Vector[XmlOrJsonOrToken] =
    Strings.totokensVector(p, "\t").map(XmlOrJsonOrToken.create)
}
