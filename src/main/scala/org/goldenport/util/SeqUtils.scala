package org.goldenport.util

import scala.language.higherKinds
import scala.language.existentials
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

/*
 * @since   Feb. 17, 2016
 *  version Mar. 24, 2017
 * @version Sep. 15, 2017
 * @author  ASAMI, Tomoharu
 */
object SeqUtils {
  def mergeStringAnyWithOption(ps: Seq[(String, Any)]*): Seq[(String, Any)] = {
    case class Z(
      r: Vector[(String, Any)] = Vector.empty,
      defined: Set[String] = Set.empty
    ) {
      def +(rhs: Seq[(String, Any)]) = (this /: rhs)(_ :+ _)
      def :+(rhs: (String, Any)) = {
        val (key, value) = rhs
        if (defined(key))
          this
        else
          value match {
            case None => this
            case Some(s) => copy(r :+ (key -> s), defined + key)
            case _ => copy(r :+ (key -> value), defined + key)
          }
      }
    }
    (Z() /: ps)(_ + _).r
  }

  // TODO buildFrom
  def removeDuplicateStrategyFirst[T](idf: T => Any)(ps: Seq[T]): Vector[T] = {
    case class Z(
      resolved: Set[Any] = Set.empty,
      entries: Vector[T] = Vector.empty
    ) {
      def r = entries // TODO
      def +(rhs: T) = {
        val id = idf(rhs)
        if (resolved.contains(id))
          Z(resolved + id, entries :+ rhs)
        else
          this
      }
    }
    ps./:(Z())(_+_).r
  }

  // def split3[T, S[_] <: Seq[T]](p: T => Boolean)(ps: S[T])(implicit bf: CanBuildFrom[Seq[_], T, S[T]]): (S[T], S[T], S[T]) = {
  //   case class Z(
  //     left: Builder[T, S[_]] = bf(),
  //     cencter: Vector[T] = Vector.empty,
  //     right: Vector[T] = Vector.empty,
  //     current: Int = 0
  //   ) {
  //     def r = (left.result(), cencter, right)
  //     def +(rhs: T) =
  //       if (_is_left) {
  //         if (p(rhs))
  //           copy(cencter = cencter :+ rhs, current = 1)
  //         else
  //           copy(left = left += rhs)
  //       } else if (_is_center) {
  //         if (p(rhs))
  //           copy(cencter = cencter :+ rhs)
  //         else
  //           copy(right = right :+ rhs, current = 2)
  //       } else {
  //         copy(right = right :+ rhs)
  //       }

  //     private def _is_left = current == 0
  //     private def _is_center = current == 1
  //     private def _is_right = !(_is_left || _is_center)
  //   }
  //   ps./:(Z())(_+_).r
  // }

  def split3V[T](p: T => Boolean)(ps: Seq[T]): (Vector[T], Vector[T], Vector[T]) = {
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

  def split3L[T](p: T => Boolean)(ps: Seq[T]): (List[T], List[T], List[T]) = {
    val (ls, cs, rs) = split3V(p)(ps)
    (ls.toList, cs.toList, rs.toList)
  }
}
