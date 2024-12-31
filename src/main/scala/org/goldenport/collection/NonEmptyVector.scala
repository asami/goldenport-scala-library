package org.goldenport.collection

import scalaz._, Scalaz._
import org.goldenport.context.Conclusion
import org.goldenport.util.VectorUtils

/*
 * @since   Dec.  8, 2018
 *  version Feb. 27, 2019
 *  version Apr. 26, 2019
 *  version May.  2, 2019
 *  version Oct. 13, 2019
 *  version Nov.  3, 2019
 *  version Apr. 18, 2020
 *  version Jun. 20, 2021
 *  version Jun.  5, 2024
 * @version Nov.  2, 2024
 * @author  ASAMI, Tomoharu
 */
case class NonEmptyVector[T](head: T, tailVector: Vector[T]) {
  lazy val vector = head +: tailVector
  lazy val list = vector.toList

  def length = tailVector.length + 1

  def apply(p: Int): T = p match {
    case 0 => head
    case _ => tailVector(p - 1)
  }
  def get(p: Int): Option[T] = p match {
    case 0 => Some(head)
    case _ => tailVector.lift(p - 1)
  }

  def tail: NonEmptyVector[T] =
    if (tailVector.isEmpty)
      Conclusion.unsupportedOperationFault("tail for empty NonEmptyVector").RAISE
    else
      NonEmptyVector(tailVector.head, tailVector.tail)

  def init: NonEmptyVector[T] =
    if (tailVector.isEmpty)
      Conclusion.unsupportedOperationFault("init for empty NonEmptyVector").RAISE
    else
      copy(tailVector = tailVector.init)
  def initVector: Vector[T] =
    if (tailVector.isEmpty)
      Vector.empty
    else
      head +: tailVector.init
  def last: T = tailVector.lastOption getOrElse head

  def :+(p: T): NonEmptyVector[T] = copy(tailVector = tailVector :+ p)
  def +:(p: T): NonEmptyVector[T] = copy(head = p, tailVector = head +: tailVector)
  def ++(ps: NonEmptyVector[T]) = copy(tailVector = (tailVector :+ ps.head) ++ ps.tailVector)
  def ++(ps: Seq[T]) = copy(tailVector = (tailVector :+ ps.head) ++ ps.tail)
  def ++:(ps: Seq[T]) = NonEmptyVector(ps ++ vector)

  def contains(p: T): Boolean = vector.contains(p)
  def exists(f: T => Boolean): Boolean = vector.exists(f)

  def map[A](f: T => A): NonEmptyVector[A] =
    NonEmptyVector(vector.map(f))
  def mapHead(f: T => T): NonEmptyVector[T] = copy(head = f(head))
  def mapTail(f: T => T): NonEmptyVector[T] = copy(tailVector = tailVector.map(f))
  def mapInit(f: T => T): NonEmptyVector[T] =
    if (tailVector.isEmpty)
      this
    else
      copy(head = f(head), tailVector = VectorUtils.mapInit(tailVector, f))

  def mapLast(f: T => T): NonEmptyVector[T] =
    if (tailVector.isEmpty)
      copy(head = f(head))
    else
      copy(tailVector = VectorUtils.mapLast(tailVector, f))

  def update(index: Int, n: T): NonEmptyVector[T] =
    if (index == 0)
      updateHead(n)
    else
      copy(tailVector = VectorUtils.update(tailVector, index - 1, n))

  def updateHead(p: T): NonEmptyVector[T] = copy(head = p)

  def updateLast(p: T): NonEmptyVector[T] =
    if (tailVector.isEmpty)
      copy(head = p)
    else
      copy(tailVector = VectorUtils.updateLast(tailVector, p))

  def replace(o: T, n: T): NonEmptyVector[T] = {
    val h = if (head == o) n else head
    val t = VectorUtils.replace(tailVector, o, n)
    NonEmptyVector(h, t)
  }
  @deprecated("Use updateHead instead.", "1.3.59")
  def replaceHead(p: T): NonEmptyVector[T] = copy(head = p)
  @deprecated("Use updateLast instead.", "1.3.59")
  def replaceTail(p: T): NonEmptyVector[T] =
    if (tailVector.isEmpty)
      copy(head = p)
    else
      copy(tailVector = VectorUtils.updateLast(tailVector, p))

  def mkString(infix: String): String = vector.mkString(infix)
  def mkString(prefix: String, infix: String, postfix: String): String = vector.mkString(postfix, infix, postfix)
}

object NonEmptyVector {
  implicit def NonEmptyVectorSemigroup[T]: Semigroup[NonEmptyVector[T]] = new Semigroup[NonEmptyVector[T]] {
    def append(lhs: NonEmptyVector[T], rhs: => NonEmptyVector[T]) = lhs ++ rhs
  }

  def apply[T](p: Seq[T]): NonEmptyVector[T] = NonEmptyVector(p.head, p.tail.toVector)
  // def apply[T](p: Vector[T]): NonEmptyVector[T] = NonEmptyVector(p.head, p.tail)
  // def apply[T](p: T, ps: T*): NonEmptyVector[T] = NonEmptyVector(p, ps.toVector)
  def apply[T](p: T): NonEmptyVector[T] = new NonEmptyVector(p, Vector.empty)
  def apply[T](p: T, ps: Seq[T]): NonEmptyVector[T] = new NonEmptyVector(p, ps.toVector)
  def apply[T](p: NonEmptyList[T]): NonEmptyVector[T] = new NonEmptyVector(p.head, p.tail.toVector)

  def create[T](p: T, ps: T*): NonEmptyVector[T] = NonEmptyVector(p, ps.toVector)

  def createOption[T](p: Iterable[T]): Option[NonEmptyVector[T]] =
    p.headOption.map(x => NonEmptyVector(x, p.tail.toVector))

  def createOption[T](p: Seq[T]): Option[NonEmptyVector[T]] =
    p.headOption.map(x => NonEmptyVector(x, p.tail.toVector))
  // def createOption[T](p: Vector[T]): Option[NonEmptyVector[T]] =
  //   p.headOption.map(x => NonEmptyVector(x, p.tail))

  def createOption[T](p: Option[Seq[T]]): Option[NonEmptyVector[T]] =
    p.flatMap(seq => seq.headOption.map(x => NonEmptyVector(x, seq.tail.toVector)))
}
