package org.goldenport.util

import scalaz._, Scalaz._

/*
 * @since   Apr. 26, 2017
 *  version Sep.  1, 2017
 *  version Oct. 17, 2018
 *  version Oct.  8, 2021
 *  version Jan. 27, 2022
 * @version Jun. 22, 2025
 * @author  ASAMI, Tomoharu
 */
object OptionUtils {
  def optionList[T](p: List[T]): Option[List[T]] = if (p.isEmpty) None else Some(p)
  def optionList[T](p: Option[List[T]]): Option[List[T]] =
    p.flatMap(optionList(_))

  private def complement[T](lhs: Option[T], rhs: Option[T]): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }

  def firstMonoid[T](lhs: Option[T], rhs: Option[T]): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(l)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }

  def lastMonoid[T](lhs: Option[T], rhs: Option[T]): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }

  // See Scalaz algebratic operation |+|.
  def append[T: Semigroup](lhs: Option[T], rhs: Option[T]): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(l |+| r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }

  def append[T](lhs: Option[T], rhs: Option[T], f: (T, T) => T): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(f(l, r))
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }

  def compareAscOption[T](lhs: Option[T], rhs: Option[T])(implicit ordering: scala.math.Ordering[T]): Option[Boolean] = {
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(ordering.gteq(l, r))
      case (Some(l), None) => Some(true)
      case (None, Some(r)) => Some(false)
      case (None, None) => None
    }
  }

  def compareDescOption[T](lhs: Option[T], rhs: Option[T])(implicit ordering: scala.math.Ordering[T]): Option[Boolean] = {
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(ordering.lteq(l, r))
      case (Some(l), None) => Some(true)
      case (None, Some(r)) => Some(false)
      case (None, None) => None
    }
  }
}
