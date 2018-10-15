package org.goldenport.values

import scalaz.NonEmptyList

/*
 * @since   Sep. 20, 2015
 *  version Jun. 16, 2016
 * @version Aug. 28, 2017
 * @author  ASAMI, Tomoharu
 */
case class ResourceName(v: String) extends AnyVal {
  override def toString() = v
}

object ResourceName {
  def onel(ps: Seq[String]): Option[NonEmptyList[ResourceName]] =
    ps.toList.map(ResourceName(_)) match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, xs))
    }
}
