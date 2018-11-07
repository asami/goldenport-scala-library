package org.goldenport.util

/*
 * @since   Apr. 26, 2017
 *  version Sep.  1, 2017
 * @version Oct. 17, 2018
 * @author  ASAMI, Tomoharu
 */
object OptionUtils {
  def optionList[T](p: List[T]): Option[List[T]] = if (p.isEmpty) None else Some(p)
  def optionList[T](p: Option[List[T]]): Option[List[T]] =
    p.flatMap(optionList(_))

  def complement[T](lhs: Option[T], rhs: Option[T]): Option[T] =
    (lhs, rhs) match {
      case (Some(l), Some(r)) => Some(r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }
}
