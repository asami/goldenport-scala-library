package org.goldenport.extension

import org.apache.commons.jxpath.DynamicPropertyHandler
import org.goldenport.RAISE

/*
 * @since   Oct. 15, 2018
 *  version Apr.  8, 2019
 *  version Jul. 24, 2019
 *  version Sep. 23, 2019
 * @version May. 19, 2020
 * @author  ASAMI, Tomoharu
 */
trait IRecord extends Showable {
  def keys: List[String] = keyNames
  def keySymbols: List[Symbol]
  def keyNames: List[String]
  def length: Int
  def get(key: Symbol): Option[Any]
  def get(key: String): Option[Any]
}

object IRecord {
  val empty = MapRecord(Map.empty)

  case class MapRecord(map: Map[Symbol, Any]) extends IRecord {
    val print = map.toString
    def display = print
    def show = print
    def embed = print
    def keySymbols: List[Symbol] = map.keySet.toList
    def keyNames: List[String] = keySymbols.map(_.name)
    def length: Int = map.size
    def get(key: Symbol): Option[Any] = map.get(key)
    def get(key: String): Option[Any] = map.get(Symbol(key))
  }
}
