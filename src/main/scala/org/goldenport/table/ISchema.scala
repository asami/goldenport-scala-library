package org.goldenport.table

import org.goldenport.RAISE

/*
 * @since   Feb. 11, 2019
 * @version Feb. 11, 2019
 * @author  ASAMI, Tomoharu
 */
trait ISchema {
  def keys: List[String]
}

case class Schema(keys: List[String]) extends ISchema {
}
