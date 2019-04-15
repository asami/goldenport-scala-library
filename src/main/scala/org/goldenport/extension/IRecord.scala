package org.goldenport.extension

import org.apache.commons.jxpath.DynamicPropertyHandler
import org.goldenport.RAISE

/*
 * @since   Oct. 15, 2018
 * @version Apr.  8, 2019
 * @author  ASAMI, Tomoharu
 */
trait IRecord {
  def keyNames: List[String]
  def get(key: String): Option[Any]
  def print: String
  def show: String
}
