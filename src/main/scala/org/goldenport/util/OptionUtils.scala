package org.goldenport.util

/*
 * @since   Apr. 26, 2017
 * @version Sep.  1, 2017
 * @author  ASAMI, Tomoharu
 */
object OptionUtils {
  def optionList[T](p: List[T]): Option[List[T]] = if (p.isEmpty) None else Some(p)
  def optionList[T](p: Option[List[T]]): Option[List[T]] =
    p.flatMap(optionList(_))
}
