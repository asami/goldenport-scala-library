package org.goldenport.matrix

import org.goldenport.RAISE

/*
 * @since   Feb. 10, 2019
 * @version Feb. 12, 2019
 * @author  ASAMI, Tomoharu
 */
trait IMatrix[T] {
  def apply(x: Int, y: Int): T
  def width: Int
  def height: Int
  def rowIterator: Iterator[Vector[T]]
  def columnIterator: Iterator[Vector[T]]
}

case class ArrayMatrix[T](matrix: Array[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
}

case class ArrayRowColumnMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
}

case class ArrayColumnRowMatrix[T](matrix: Array[Array[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y)
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = matrix.iterator.map(_.toVector)
}

case class VectorMatrix[T](matrix: Vector[T], width: Int, height: Int) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y * width + x)
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
}

case class VectorRowColumnMatrix[T](matrix: Vector[Vector[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(y)(x)
  lazy val width: Int = matrix.map(_.length).max
  def height: Int = matrix.length
  def rowIterator: Iterator[Vector[T]] = matrix.iterator
  def columnIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
}
object VectorRowColumnMatrix {
  def create[T](data: Seq[Seq[T]]): VectorRowColumnMatrix[T] =
    VectorRowColumnMatrix(data.toVector.map(_.toVector))
}

case class VectorColumnRowMatrix[T](matrix: Vector[Vector[T]]) extends IMatrix[T] {
  def apply(x: Int, y: Int): T = matrix(x)(y)
  def width: Int = matrix.length
  lazy val height: Int = matrix.map(_.length).max
  def rowIterator: Iterator[Vector[T]] = RAISE.notImplementedYetDefect
  def columnIterator: Iterator[Vector[T]] = matrix.iterator
}
