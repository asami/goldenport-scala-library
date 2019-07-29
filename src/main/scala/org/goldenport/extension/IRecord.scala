package org.goldenport.extension

import org.apache.commons.jxpath.DynamicPropertyHandler
import org.goldenport.RAISE

/*
 * @since   Oct. 15, 2018
 *  version Apr.  8, 2019
 * @version Jul. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait IRecord extends Showable {
  def keys: List[Symbol]
  def keyNames: List[String]
  def length: Int
  def get(key: Symbol): Option[Any]
  def get(key: String): Option[Any]
}
