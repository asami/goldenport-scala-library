package org.goldenport.matrix

import org.goldenport.RAISE
import org.goldenport.extension.Showable
import org.goldenport.matrix.breeze.BreezeMatrix

/*
 * @since   Feb. 10, 2019
 *  version Jun. 30, 2019
 *  version Jul. 16, 2019
 * @version Aug. 26, 2019
 * @author  ASAMI, Tomoharu
 */
trait IMatrix[T] extends Showable {
  def apply(x: Int, y: Int): T
  def width: Int
  def height: Int
  def rowIterator: Iterator[Vector[T]]
  def columnIterator: Iterator[Vector[T]]
  // 
  def appendRow(p: Seq[T]): IMatrix[T]
  def appendRows(ps: IMatrix[T]): IMatrix[T]
  //
  def print = show
  def display = s"Matrix[${width}x${height}]"
  def show = MatrixVisualizer.linearAlgebra.plainText(this)
  //
  def transpose: IMatrix[T]
  def +(rhs: IMatrix[Double])(implicit ev: T <:< Double): IMatrix[Double] = BreezeMatrix.create(this.asInstanceOf[IMatrix[Double]]) + BreezeMatrix.create(rhs)
}

case class ArrayMatrix[T](matrix: Array[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class ArrayRowColumnMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class ArrayColumnRowMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y)
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class VectorMatrix[T](matrix: Vector[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.unsupportedOperationFault
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

trait VectorRowColumnMatrixBase[T] extends IMatrix[T] {
  def matrix: Vector[Vector[T]]

  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect

  def transpose: IMatrix[T] = VectorColumnRowMatrix(matrix)
}

case class VectorRowColumnMatrix[T](matrix: Vector[Vector[T]]) extends VectorRowColumnMatrixBase[T] {
  def appendRow(ps: Seq[T]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(matrix :+ ps.toVector)

  def appendRows(ps: IMatrix[T]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(matrix ++ ps.rowIterator)
}
object VectorRowColumnMatrix {
  private val _empty = VectorRowColumnMatrix(Vector.empty)

  def empty[T] = _empty.asInstanceOf[VectorRowColumnMatrix[T]]

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

  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): VectorRowColumnMatrix[T] = RAISE.notImplementedYetDefect
  def transpose: IMatrix[T] = VectorRowColumnMatrix(matrix)
}
object VectorColumnRowMatrix {
  def apply[T](p: Seq[Seq[T]], empty: T): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector), Some(empty))

  def create[T](p: Seq[Seq[T]]): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector))
}
