package org.goldenport.collection

import scalaz._, Scalaz._
import org.goldenport.util.VectorUtils

/*
 * @since   Dec.  8, 2018
 *  version Feb. 27, 2019
 *  version Apr. 26, 2019
 *  version May.  2, 2019
 *  version Oct. 13, 2019
 * @version Nov.  3, 2019
 * @author  ASAMI, Tomoharu
 */
case class NonEmptyVector[T](head: T, tail: Vector[T]) {
  lazy val vector = head +: tail
  lazy val list = vector.toList

  def length = tail.length + 1

  def apply(p: Int): T = p match {
    case 0 => head
    case _ => tail(p - 1)
  }
  def get(p: Int): Option[T] = p match {
    case 0 => Some(head)
    case _ => tail.lift(p - 1)
  }

  def init = if (tail.isEmpty) head else head +: tail.init
  def last = tail.lastOption getOrElse head

  def :+(p: T): NonEmptyVector[T] = copy(tail = tail :+ p)
  def +:(p: T): NonEmptyVector[T] = copy(head = p, tail = head +: tail)
  def ++(ps: NonEmptyVector[T]) = copy(tail = (tail :+ ps.head) ++ ps.tail)
  def ++(ps: Seq[T]) = copy(tail = (tail :+ ps.head) ++ ps.tail)

  def exists(f: T => Boolean): Boolean = vector.exists(f)

  def map[A](f: T => A): NonEmptyVector[A] =
    NonEmptyVector(vector.map(f))
  def mapHead(f: T => T): NonEmptyVector[T] = copy(head = f(head))
  def mapTail(f: T => T): NonEmptyVector[T] = copy(tail = tail.map(f))
  def mapInit(f: T => T): NonEmptyVector[T] =
    if (tail.isEmpty)
      this
    else
      copy(head = f(head), tail = VectorUtils.mapInit(tail, f))

  def mapLast(f: T => T): NonEmptyVector[T] =
    if (tail.isEmpty)
      copy(head = f(head))
    else
      copy(tail = VectorUtils.mapLast(tail, f))

  def replace(o: T, n: T): NonEmptyVector[T] = {
    val h = if (head == o) n else head
    val t = VectorUtils.replace(tail, o, n)
    NonEmptyVector(h, t)
  }
  def replaceHead(p: T): NonEmptyVector[T] = copy(head = p)
  def replaceTail(p: T): NonEmptyVector[T] =
    if (tail.isEmpty)
      copy(head = p)
    else
      copy(tail = VectorUtils.replaceTail(tail, p))
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

  def create[T](p: T, ps: T*): NonEmptyVector[T] = NonEmptyVector(p, ps.toVector)

  def createOption[T](p: Seq[T]): Option[NonEmptyVector[T]] =
    p.headOption.map(x => NonEmptyVector(x, p.tail.toVector))
  // def createOption[T](p: Vector[T]): Option[NonEmptyVector[T]] =
  //   p.headOption.map(x => NonEmptyVector(x, p.tail))

  def createOption[T](p: Option[Seq[T]]): Option[NonEmptyVector[T]] =
    p.flatMap(seq => seq.headOption.map(x => NonEmptyVector(x, seq.tail.toVector)))
}
