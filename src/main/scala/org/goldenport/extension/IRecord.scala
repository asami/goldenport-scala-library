package org.goldenport.extension

import org.apache.commons.jxpath.DynamicPropertyHandler
import org.goldenport.RAISE
import org.goldenport.collection.VectorMap

/*
 * @since   Oct. 15, 2018
 *  version Apr.  8, 2019
 *  version Jul. 24, 2019
 *  version Sep. 23, 2019
 *  version May. 19, 2020
 *  version Oct. 18, 2020
 *  version Mar. 28, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
trait IRecord extends Showable {
  def keys: List[String] = keyNames
  def keySymbols: List[Symbol]
  def keyNames: List[String]
  def length: Int
  def get(key: Symbol): Option[Any]
  def get(key: String): Option[Any]
  def toMapS: Map[Symbol, Any] = keySymbols.flatMap(x => get(x).map(y => x -> y)).toMap
  def toMap: Map[String, Any] = keyNames.flatMap(x => get(x).map(y => x -> y)).toMap
  def +(rhs: IRecord): IRecord
}

object IRecord {
  val empty = VectorMapRecord(VectorMap.empty)

  case class MapRecord(map: Map[Symbol, Any]) extends IRecord {
    val print = map.toString
    def display = print
    def show = print
    def keySymbols: List[Symbol] = map.keySet.toList
    def keyNames: List[String] = keySymbols.map(_.name)
    def length: Int = map.size
    def get(key: Symbol): Option[Any] = map.get(key)
    def get(key: String): Option[Any] = map.get(Symbol(key))
    def +(rhs: IRecord): IRecord = MapRecord(map ++ rhs.toMapS)
    override def toMapS = map
    override def toMap = map.map {
      case (k, v) => k.name -> v
    }
  }

  case class VectorMapRecord(map: VectorMap[Symbol, Any]) extends IRecord {
    val print = map.toString
    def display = print
    def show = print
    def keySymbols: List[Symbol] = map.keySet.toList
    def keyNames: List[String] = keySymbols.map(_.name)
    def length: Int = map.size
    def get(key: Symbol): Option[Any] = map.get(key)
    def get(key: String): Option[Any] = map.get(Symbol(key))
    def +(rhs: IRecord): IRecord = MapRecord(map ++ rhs.toMapS)
    override def toMapS = map
    override def toMap = map.map {
      case (k, v) => k.name -> v
    }
  }

  def createS(ps: Seq[(Symbol, Any)]): IRecord = VectorMapRecord(VectorMap(ps))

  def create(ps: Seq[(String, Any)]): IRecord = createS(ps.map {
    case (k, v) => Symbol(k) -> v
  })

  def createS(p: Map[Symbol, Any]): IRecord = MapRecord(p)

  def create(p: Map[String, Any]): IRecord = createS(p.map {
    case (k, v) => Symbol(k) -> v
  })

  def dataS(p: (Symbol, Any), ps: Tuple2[Symbol, Any]*): IRecord = createS(p +: ps)

  def data(p: (String, Any), ps: Tuple2[String, Any]*): IRecord = create(p +: ps)

  def dataOptionS(p: (Symbol, Option[Any]), ps: Tuple2[Symbol, Option[Any]]*): IRecord = createS(_reduce(p +: ps))

  def dataOption(p: (String, Option[Any]), ps: Tuple2[String, Option[Any]]*): IRecord = create(_reduce(p +: ps))

  private def _reduce[K, V](ps: Seq[(K, Option[V])]): Seq[(K, V)] =
    ps.flatMap(x => x._2.map(v => x._1 -> v))
}
