package org.goldenport.context

import scalaz._, Scalaz._

/*
 * @since   Sep.  1, 2022
 * @version Sep.  1, 2022
 * @author  ASAMI, Tomoharu
 */
case class ConsequenceSequence[T](
  consequences: Vector[Consequence[T]] = Vector.empty
) {
  def +(p: ConsequenceSequence[T]) = copy(consequences ++ p.consequences)
  def +(ps: Iterable[Consequence[T]]) = copy(consequences ++ ps)
  def +(p: Consequence[T]) = copy(consequences :+ p)

  def isSuccess: Boolean = consequences.forall(_.isSuccess)
  def isError: Boolean = !isSuccess

  def success: Vector[T] = consequences.collect {
    case Consequence.Success(s, _) => s
  }
  def error: Vector[Consequence.Error[T]] = consequences.collect {
    case m: Consequence.Error[T] => m
  }

  def toConsequence: Consequence[Vector[T]] = consequences.sequence
}
object ConsequenceSequence {
  private val _empty = ConsequenceSequence()

  def empty[T] = _empty.asInstanceOf[ConsequenceSequence[T]]

  implicit def ConsequenceSequenceMonoid[T] = new Monoid[ConsequenceSequence[T]] {
    def zero = empty
    def append(lhs: ConsequenceSequence[T], rhs: => ConsequenceSequence[T]) =
      ConsequenceSequence(lhs.consequences ++ rhs.consequences)
  }
}

