package org.goldenport.util

/*
 * @since   Jul. 16, 2018
 * @version Aug. 26, 2018
 * @author  ASAMI, Tomoharu
 */
object VectorUtils {
  def buildTupleVector[T](fixed: Seq[(String, T)], options: Seq[(String, Option[T])]): Vector[(String, T)] =
    SeqUtils.buildTupleVector(fixed, options)

  def buildTupleVector[T](p: (String, Option[T]), ps: (String, Option[T])*): Vector[(String, T)] =
    SeqUtils.buildTupleVector(p +: ps)

  def buildTupleVector[T](options: Seq[(String, Option[T])]): Vector[(String, T)] =
    SeqUtils.buildTupleVector(options)

  def sliding2[T](ps: Seq[T]): Vector[Seq[T]] =
    ps.length match {
      case 0 => Vector.empty
      case 1 => Vector(Vector(ps(0)))
      case _ =>
        val a = ps.sliding(2, 1).toVector
        a.lastOption.map(x => a :+ Vector(x(1))).getOrElse(a)
    }

  def sliding3[T](ps: Seq[T]): Vector[Seq[T]] =
    ps.length match {
      case 0 => Vector.empty
      case 1 => Vector(Vector(ps(0)))
      case 2 => Vector(Vector(ps(0), ps(1)), Vector(ps(1)))
      case _ => 
        val a = ps.sliding(3, 1).toVector
        a.lastOption.map(x =>
          a :+ Vector(x(1), x(2)) :+ Vector(x(2))
        ).getOrElse(a)
    }
}
