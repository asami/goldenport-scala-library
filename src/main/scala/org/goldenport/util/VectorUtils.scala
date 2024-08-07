package org.goldenport.util

/*
 * @since   Jul. 16, 2018
 *  version Aug. 26, 2018
 *  version Dec. 27, 2018
 *  version Jul. 29, 2019
 *  version Jan. 31, 2020
 *  version Oct.  1, 2021
 * @version Jun.  5, 2024
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

  def sliding4[T](ps: Seq[T]): Vector[Seq[T]] =
    ps.length match {
      case 0 => Vector.empty
      case 1 => Vector(Vector(ps(0)))
      case 2 => Vector(Vector(ps(0), ps(1)), Vector(ps(1)))
      case 3 => Vector(Vector(ps(0), ps(1), ps(2)), Vector(ps(1), ps(2)), Vector(ps(2)))
      case _ => 
        val a = ps.sliding(4, 1).toVector
        a.lastOption.map(x =>
          a :+ Vector(x(1), x(2), x(3)) :+ Vector(x(2), x(3)) :+ Vector(x(3))
        ).getOrElse(a)
    }

  def mapHead[T](v: Vector[T], f: T => T): Vector[T] =
    v.headOption.
      map(x => f(x) +: v.tail).
      getOrElse(v)

  def mapTail[T](v: Vector[T], f: T => T): Vector[T] =
    v.headOption.
      map(x => x +: v.tail.map(f)).
      getOrElse(v)

  def mapInit[T](v: Vector[T], f: T => T): Vector[T] =
    v.lastOption.
      map(x => v.init.map(f) :+ x).
      getOrElse(v)

  def mapLast[T](v: Vector[T], f: T => T): Vector[T] =
    v.lastOption.
      map(x => v.init :+ f(x)).
      getOrElse(v)

  def update[T](v: Vector[T], index: Int, p: T): Vector[T] = v.updated(index, p)

  def updateHead[T](v: Vector[T], p: T): Vector[T] =
    if (v.isEmpty)
      v
    else
      v.updated(0, p)

  def updateLast[T](v: Vector[T], p: T): Vector[T] = 
    if (v.isEmpty)
      v
    else
      v.updated(v.length - 1, p)

  def replace[T](v: Vector[T], o: T, n: T): Vector[T] =
    v.map(x => if (x == o) n else x)

  @deprecated("Use updateHead instead.", "1.3.59")
  def replaceHead[T](v: Vector[T], p: T): Vector[T] =
    if (v.isEmpty)
      v
    else
      v.updated(0, p)

  @deprecated("Use update instead.", "1.3.59")
  def replaceTail[T](v: Vector[T], p: T): Vector[T] = 
    if (v.isEmpty)
      v
    else
      v.updated(v.length - 1, p)

  def updateMap[K, V](v: Vector[(K, V)], kv: (K, V)): Vector[(K, V)] =
    updateMap(v, kv._1, kv._2)

  def updateMap[K, V](p: Vector[(K, V)], k: K, v: V): Vector[(K, V)] =
    p.indexWhere(_._1 == k) match {
      case -1 => p :+ (k -> v)
      case i => p.updated(i, (k -> v))
    }

  def updateMap[K, V](v: Vector[(K, V)], kvs: Map[K, V]): Vector[(K, V)] =
    updateMap(v, kvs.toSeq)

  def updateMap[K, V](v: Vector[(K, V)], kvs: Seq[(K, V)]): Vector[(K, V)] =
    kvs./:(v)(updateMap(_, _))

  def removeMap[K, V](p: Vector[(K, V)], k: K): Vector[(K, V)] =
    p.filterNot(_._1 == k)

  def split3[T](p: T => Boolean)(ps: Seq[T]): (Vector[T], Vector[T], Vector[T]) = {
    val LEFT = 0
    val CENTER = 1
    val RIGHT = 2
    case class Z(
      left: Vector[T] = Vector.empty,
      cencter: Vector[T] = Vector.empty,
      right: Vector[T] = Vector.empty,
      current: Int = LEFT
    ) {
      def r = (left, cencter, right)
      def +(rhs: T) =
        if (_is_left) {
          if (p(rhs))
            copy(cencter = cencter :+ rhs, current = CENTER)
          else
            copy(left = left :+ rhs)
        } else if (_is_center) {
          if (p(rhs))
            copy(cencter = cencter :+ rhs)
          else
            copy(right = right :+ rhs, current = RIGHT)
        } else {
          copy(right = right :+ rhs)
        }

      private def _is_left = current == LEFT
      private def _is_center = current == CENTER
      private def _is_right = !(_is_left || _is_center)
    }
    ps./:(Z())(_+_).r
  }

  def zipRightOption[A, B](pl: Seq[A], pr: Seq[B]): Vector[(A, Option[B])] = {
    case class Z(
      rs: List[B] = pr.toList,
      r: Vector[(A, Option[B])] = Vector.empty
    ) {
      def +(rhs: A) = rs.headOption match {
        case Some(s) => copy(rs = rs.tail, r = r :+ (rhs, Some(s)))
        case None => copy(r = r :+ (rhs, None))
      }
    }
    pl./:(Z())(_+_).r
  }
}
