package org.goldenport.util

/*
 * @since   May. 11, 2018
 *  version Jun. 29, 2018
 *  version Jul. 16, 2018
 * @version Aug.  5, 2018
 * @author  ASAMI, Tomoharu
 */
object ListUtils {
  def toOption[T](ps: List[T]): Option[List[T]] = ps match {
    case Nil => None
    case xs => Some(xs)
  }

  def toOptionOneOrList[T](ps: List[T]): Option[Any] = ps match {
    case Nil => None
    case x :: Nil => Some(x)
    case xs =>  Some(xs)
  }

  def buildTupleList[T](fixed: Seq[(String, T)], options: Seq[(String, Option[T])]): List[(String, T)] =
    SeqUtils.buildTupleList(fixed, options)

  def buildTupleList[T](p: (String, Option[T]), ps: (String, Option[T])*): List[(String, T)] =
    SeqUtils.buildTupleList(p +: ps)

  def buildTupleList[T](options: Seq[(String, Option[T])]): List[(String, T)] =
    SeqUtils.buildTupleList(options)
}
