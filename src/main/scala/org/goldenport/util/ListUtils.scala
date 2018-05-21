package org.goldenport.util

/*
 * @since   May. 11, 2018
 * @version May. 11, 2018
 * @author  ASAMI, Tomoharu
 */
object ListUtils {
  def toOptionOneOrList[T](ps: List[T]): AnyRef = ps match {
    case Nil => None
    case x :: Nil => Some(x)
    case xs =>  Some(xs)
  }
}
