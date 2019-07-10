package org.goldenport.matrix

import org.goldenport.RAISE

/*
 * @since   Feb. 10, 2019
 *  version Jun. 30, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
trait IMatrix[T] {
  def apply(x: Int, y: Int): T
  def width: Int
  def height: Int
  def rowIterator: Iterator[Vector[T]]
  def columnIterator: Iterator[Vector[T]]
  // 
  def appendRows(ps: IMatrix[T]): IMatrix[T]
}

case class ArrayMatrix[T](matrix: Array[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
}

case class ArrayRowColumnMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
}

case class ArrayColumnRowMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y)
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
}

case class VectorMatrix[T](matrix: Vector[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
}

trait VectorRowColumnMatrixBase[T] extends IMatrix[T] {
  def matrix: Vector[Vector[T]]

  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
}

case class VectorRowColumnMatrix[T](matrix: Vector[Vector[T]]) extends VectorRowColumnMatrixBase[T] {
  def appendRows(ps: IMatrix[T]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(matrix ++ ps.rowIterator)
}
object VectorRowColumnMatrix {
  def create[T](data: Seq[Seq[T]]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(data.toVector.map(_.toVector))
}

case class VectorColumnRowMatrix[T](
  matrix: Vector[Vector[T]],
  emptyValue: Option[T] = None
) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y) // TODO empty value
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = matrix.iterator

  def appendRows(ps: IMatrix[T]): VectorRowColumnMatrix[T] = RAISE.notImplementedYetDefect
}
object VectorColumnRowMatrix {
  def apply[T](p: Seq[Seq[T]], empty: T): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector), Some(empty))

  def create[T](p: Seq[Seq[T]]): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector))
}
