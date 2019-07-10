package org.goldenport.tsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.matrix._
import org.goldenport.parser.{LogicalParagraph, LogicalLine, XmlOrJsonOrToken}

/*
 * @since   Jul.  7, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class XJTTsv(matrix: VectorRowColumnMatrix[XmlOrJsonOrToken]) extends IMatrix[XmlOrJsonOrToken] {
  def apply(x: Int, y: Int): XmlOrJsonOrToken = matrix.apply(x, y)
  def width: Int = matrix.width
  def height: Int = matrix.height
  def rowIterator: Iterator[Vector[XmlOrJsonOrToken]] = matrix.rowIterator
  def columnIterator: Iterator[Vector[XmlOrJsonOrToken]] = matrix.columnIterator
  def appendRows(ps: IMatrix[XmlOrJsonOrToken]): IMatrix[XmlOrJsonOrToken] = RAISE.unsupportedOperationFault
}

object XJTTsv {
  def create(p: LogicalParagraph): XJTTsv = create(p.lines.lines)

  def create(ps: Vector[LogicalLine]): XJTTsv =
    XJTTsv(VectorRowColumnMatrix(ps.map(createRow)))

  def createRow(p: LogicalLine): Vector[XmlOrJsonOrToken] = createRow(p.text)

  def createRow(p: String): Vector[XmlOrJsonOrToken] =
    Strings.totokensVector(p, "\t").map(XmlOrJsonOrToken.create)
}
