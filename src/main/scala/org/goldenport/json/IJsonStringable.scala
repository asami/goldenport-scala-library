package org.goldenport.json

/*
 * @since   Sep.  2, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
trait IJsonStringable {
  def toJsonString(buf: StringBuilder): Unit
}
