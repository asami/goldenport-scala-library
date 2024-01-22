package org.goldenport.context

import org.goldenport.value._

/*
 * @since   Mar. 15, 2021
 * @version Jun. 13, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait CacheStrategy extends NamedValueInstance {
}

object CacheStrategy extends EnumerationClass[CacheStrategy] {
  val none = NoneCache

  val elements = Vector(NoneCache)

  case object NoneCache extends CacheStrategy {
    val name = "none"
  }
}
