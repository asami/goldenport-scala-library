package org.goldenport.tsv

import scalaz._, Scalaz._
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.matrix._

/*
 * @since   Jul.  7, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class Tsv(matrix: VectorRowColumnMatrix[String]) extends IMatrix[String] {
  def apply(x: Int, y: Int): String = ???
  def width: Int = ???
  def height: Int = ???
  def rowIterator: Iterator[Vector[String]] = ???
  def columnIterator: Iterator[Vector[String]] = ???
  def appendRows(ps: IMatrix[String]): IMatrix[String] = RAISE.unsupportedOperationFault
}
