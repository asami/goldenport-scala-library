package org.goldenport.matrix

import org.goldenport.RAISE
import org.goldenport.extension.Showable
import org.goldenport.values.NumberRange
import org.goldenport.matrix.breeze.BreezeMatrix

/*
 * @since   Feb. 10, 2019
 *  version Jun. 30, 2019
 *  version Jul. 16, 2019
 *  version Aug. 26, 2019
 *  version Sep. 16, 2019
 * @version Oct.  8, 2019
 * @author  ASAMI, Tomoharu
 */
trait IMatrix[T] extends Showable {
  def apply(x: Int, y: Int): T
  def width: Int
  def height: Int
  def rowIterator: Iterator[Vector[T]] = new Iterator[Vector[T]] {
    var y = 0
    def hasNext: Boolean = height > y
    def next(): Vector[T] = {
      val xs = for (x <- 0 until width) yield { apply(x, y) }
      y += 1
      xs.toVector
    }
  }
  def columnIterator: Iterator[Vector[T]] = new Iterator[Vector[T]] {
    var x = 0
    def hasNext: Boolean = width > x
    def next(): Vector[T] = {
      val xs = for (y <- 0 until height) yield { apply(x, y) }
      x += 1
      xs.toVector
    }
  }
  def projection(p: NumberRange): IMatrix[T]
  def selection(p: NumberRange): IMatrix[T]
  def toDoubleMatrix: IMatrix[Double]
  def makeDoubleMatrix: IMatrix[Double]
  // 
  def appendRow(p: Seq[T]): IMatrix[T]
  def appendRows(ps: IMatrix[T]): IMatrix[T]
  def appendColumn(p: Seq[T]): IMatrix[T]
  def appendColumns(p: IMatrix[T]): IMatrix[T]
  //
  def print = show
  def display = s"Matrix[${width}x${height}]"
  def show = MatrixVisualizer.linearAlgebra.plainText(this)
  //
  def transpose: IMatrix[T]
  def inv(implicit ev: T <:< Double): IMatrix[Double] = to_breeze_matrix.inv
  def det(implicit ev: T <:< Double): Double = to_breeze_matrix.det
  def rank(implicit ev: T <:< Double): Int = to_breeze_matrix.rank
  def +(rhs: IMatrix[Double])(implicit ev: T <:< Double): IMatrix[Double] = to_breeze_matrix + BreezeMatrix.create(rhs)
  def -(rhs: IMatrix[Double])(implicit ev: T <:< Double): IMatrix[Double] = to_breeze_matrix - BreezeMatrix.create(rhs)
  def *(rhs: IMatrix[Double])(implicit ev: T <:< Double): IMatrix[Double] = to_breeze_matrix * BreezeMatrix.create(rhs)
  def *(rhs: Double)(implicit ev: T <:< Double): IMatrix[Double] = to_breeze_matrix * rhs
  //
  protected def to_breeze_matrix: BreezeMatrix = BreezeMatrix.create(this.asInstanceOf[IMatrix[Double]])
}

case class ArrayMatrix[T](matrix: Array[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumns(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class ArrayRowColumnMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  override def rowIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumns(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class ArrayColumnRowMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y)
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  override def columnIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumns(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

case class VectorMatrix[T](matrix: Vector[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumns(ps: IMatrix[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def transpose: IMatrix[T] = RAISE.notImplementedYetDefect
}

trait VectorRowColumnMatrixBase[T] extends IMatrix[T] {
  def matrix: Vector[Vector[T]]
  def emptyValue: Option[T]

  def apply(x: Int, y: Int): T = {
    if (y >= height || x >= width)
      throw new IndexOutOfBoundsException(s"${getClass.getSimpleName}[$width, $height]: $x, $y")
    val xs = matrix(y)
    if (x >= xs.length)
      emptyValue getOrElse RAISE.illegalStateFault(s"${getClass.getSimpleName}[$width, $height]: $x, $y: empty value undefined")
    else
      xs(x)
  }
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  override def rowIterator: Iterator[Vector[T]] = matrix.iterator
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect

  def transpose: IMatrix[T] = VectorColumnRowMatrix(matrix)
}

case class VectorRowColumnMatrix[T](
  matrix: Vector[Vector[T]],
  emptyValue: Option[T] = None
) extends VectorRowColumnMatrixBase[T] {
  override def projection(p: NumberRange): IMatrix[T] =
    VectorRowColumnMatrix(MatrixUtils.projectionVector(matrix, p))

  override def toDoubleMatrix: IMatrix[Double] =
    VectorRowColumnMatrix(MatrixUtils.toDoubleVector(matrix))

  override def makeDoubleMatrix: IMatrix[Double] =
    VectorRowColumnMatrix(MatrixUtils.makeProjectionDoubleVector(matrix))

  def appendRow(ps: Seq[T]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(matrix :+ ps.toVector)

  def appendRows(ps: IMatrix[T]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(matrix ++ ps.rowIterator)

  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect

  def appendColumns(ps: IMatrix[T]): IMatrix[T] = {
    val a = matrix.zip(ps.rowIterator.toVector).map {
      case (l, r) => l ++ r
    }
    VectorRowColumnMatrix(a)
  }
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
  def apply(x: Int, y: Int): T = {
    if (y >= height || x >= width)
      throw new IndexOutOfBoundsException(s"${getClass.getSimpleName}[$width, $height]: $x, $y")
    val xs = matrix(x)
    if (y >= xs.length)
      emptyValue getOrElse RAISE.illegalStateFault(s"${getClass.getSimpleName}[$width, $height]: $x, $y: empty value undefined")
    else
      xs(y)
  }
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  override def columnIterator: Iterator[Vector[T]] = matrix.iterator
  def projection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def selection(p: NumberRange): IMatrix[T] = RAISE.notImplementedYetDefect
  def toDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect
  def makeDoubleMatrix: IMatrix[Double] = RAISE.notImplementedYetDefect

  def appendRow(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendRows(p: IMatrix[T]): VectorRowColumnMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumn(ps: Seq[T]): IMatrix[T] = RAISE.notImplementedYetDefect
  def appendColumns(p: IMatrix[T]): IMatrix[T] = {
    val a = matrix ++ p.columnIterator.toVector
    VectorColumnRowMatrix(a)
  }
  def transpose: IMatrix[T] = VectorRowColumnMatrix(matrix)
}
object VectorColumnRowMatrix {
  def apply[T](p: Seq[Seq[T]], empty: T): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector), Some(empty))

  def create[T](p: Seq[Seq[T]]): VectorColumnRowMatrix[T] =
    new VectorColumnRowMatrix(p.toVector.map(_.toVector))
}
