package org.goldenport.context

/*
 * @since   Mar. 15, 2021
 * @version Mar. 15, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait CacheStrategy {
}

object CacheStrategy {
  val none = NoneCache

  case object NoneCache extends CacheStrategy
}
