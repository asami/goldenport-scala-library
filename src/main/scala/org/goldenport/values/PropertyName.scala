package org.goldenport.values

import scalaz.{NonEmptyList, IList}

/*
 * @since   May.  5, 2017
 *  version Aug. 29, 2017
 * @version Oct. 15, 2018
 * @author  ASAMI, Tomoharu
 */
case class PropertyName(v: String) extends AnyVal {
  override def toString() = v
}

object PropertyName {
  def onel(ps: Seq[String]): Option[NonEmptyList[PropertyName]] =
    ps.toList.map(PropertyName(_)) match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, IList.fromList(xs)))
    }
}
