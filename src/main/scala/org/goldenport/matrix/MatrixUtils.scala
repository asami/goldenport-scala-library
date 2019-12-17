package org.goldenport.matrix

import org.goldenport.RAISE
import org.goldenport.util.{AnyRefUtils, StringUtils}
import org.goldenport.values.NumberRange

/*
 * @since   Aug. 27, 2019
 * @version Sep. 16, 2019
 * @author  ASAMI, Tomoharu
 */
object MatrixUtils {
  def xToXy(f: Double => Double)(in: Seq[Double]): IMatrix[Double] = {
    val a = in.map(x => Vector(x, f(x)))
    VectorRowColumnMatrix(a.toVector)
  }

  def xToLine(f: Double => Double)(domain: Seq[Double]): IMatrix[Double] = {
    val from = domain.min
    val to = domain.max
    val by = 0.1
    xToSequence(f)(from, to, by)
  }

  def xToSequence(f: Double => Double)(from: Double, to: Double, step: Double): IMatrix[Double] = {
    val a = for (i <- from to to by step) yield Vector(i, f(i))
    VectorRowColumnMatrix(a.toVector)
  }

  def projectionVector[T](ps: Vector[Vector[T]], range: NumberRange): Vector[Vector[T]] =
    ps.map(_.zipWithIndex.flatMap {
      case (x, i) => if (range.isValid(i)) Some(x) else None
    })

  def selectionVector[T](ps: Vector[Vector[T]], range: NumberRange): Vector[Vector[T]] =
    ps.zipWithIndex.flatMap {
      case (x, i) => if (range.isValid(i)) Some(x) else None
    }

  def toDoubleVector[T](ps: Vector[Vector[T]]): Vector[Vector[Double]] =
    ps.map(_.map(x => AnyRefUtils.toDouble(x): Double))

  def makeProjectionDoubleVector[T](ps: Vector[Vector[T]]): Vector[Vector[Double]] = {
    def init(xs: Vector[T]): Vector[Int] = xs.zipWithIndex.flatMap {
      case (x, i) => if (_is_double(x)) Some(i) else None
    }

    case class Z(actives: Vector[Int]) {
      def r: Vector[Vector[Double]] = ps.map(x =>
        actives.map(i => AnyRefUtils.toDouble(x(i)): Double)
      )

      def +(rhs: Vector[T]) = if (actives.isEmpty) {
        this
      } else {
        val a = actives.filter(i => rhs.lift(i).map(_is_double).getOrElse(false))
        Z(a)
      }
    }

    ps.headOption.
      map(x => ps.tail./:(Z(init(x)))(_+_).r).
      getOrElse(Vector.empty)
  }

  private def _is_double(p: Any) = p match {
    case _: Byte => true
    case _: Short => true
    case _: Int => true
    case _: Long => true
    case _: Float => true
    case _: Double => true
    case _: BigInt => true
    case _: BigDecimal => true
    case m: String => StringUtils.doubleOption(m).isDefined
    case _ => false
  }
}
