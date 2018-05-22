package org.goldenport.util

/*
 * @since   May. 11, 2018
 * @version May. 23, 2018
 * @author  ASAMI, Tomoharu
 */
object ListUtils {
  def toOption[T](ps: List[T]): Option[List[T]] = ps match {
    case Nil => None
    case xs => Some(xs)
  }

  def toOptionOneOrList[T](ps: List[T]): AnyRef = ps match {
    case Nil => None
    case x :: Nil => Some(x)
    case xs =>  Some(xs)
  }
}
