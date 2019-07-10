package org.goldenport.collection

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.util.VectorUtils

/*
 * @since   Dec.  8, 2018
 *  version Dec. 30, 2018
 *  version Mar. 24, 2019
 *  version May. 10, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait VectorMap[K, +V] extends Map[K, V] {
  def ++[W >: V](p: VectorMap[K, W]): VectorMap[K, W]
  override def +[W >: V](kv: (K, W)): VectorMap[K, W]
  override def -(key: K): VectorMap[K, V]
  override def toVector = vector
  def vector: Vector[(K, V)]
  override def toList: List[(K, V)] = vector.toList
  lazy val list: List[(K, V)] = toList
  def toValues: Vector[V] = vector.map(_._2)
  lazy val valueVector: Vector[V] = toValues
  def toValueList: List[V] = vector.map(_._2).toList
  lazy val valueList: List[V] = toValueList
  def update[W >: V](k: K, v: W): VectorMap[K, W] = update(k -> v)
  def update[W >: V](p: (K, W)): VectorMap[K, W]
  def update[W >: V](p: Map[K, W]): VectorMap[K, W]
  def append[W >: V](k: K, v: W): VectorMap[K, W] = append(k -> v)
  def append[W >: V](p: (K, W)): VectorMap[K, W] = remove(p._1).update(p)
  def prepend[W >: V](k: K, v: W): VectorMap[K, W] = prepend(k -> v)
  def prepend[W >: V](p: (K, W)): VectorMap[K, W] = RAISE.notImplementedYetDefect
  def remove(k: K): VectorMap[K, V]

  protected def update_vector[W >: V](ps: Vector[(K, W)], x: (K, W)): Vector[(K, W)] =
    VectorUtils.updateMap(ps, x)

  protected def update_vector[W >: V](ps: Vector[(K, W)], xs: Map[K, W]): Vector[(K, W)] =
    VectorUtils.updateMap(ps, xs)

  protected def update_vector[W >: V](ps: Vector[(K, W)], xs: Seq[(K, W)]): Vector[(K, W)] =
    VectorUtils.updateMap(ps, xs)

  def applyIgnoreCase(k: String)(implicit ev: K <:< String): V =
    getIgnoreCase(k) getOrElse RAISE.noSuchElementFault(k)

  def getIgnoreCase(k: String)(implicit ev: K <:< String): Option[V] =
    vector.find(_._1.equalsIgnoreCase(k)).map(_._2)
}

case class PlainVectorMap[K, +V](
  vector: Vector[(K, V)]
) extends VectorMap[K, V] {
  def ++[W >: V](p: VectorMap[K, W]): VectorMap[K, W] = PlainVectorMap(vector ++ p.vector)
  def +[W >: V](kv: (K, W)): VectorMap[K, W] =
    PlainVectorMap(VectorUtils.updateMap(vector, kv))
  def -(key: K): VectorMap[K, V] = PlainVectorMap(VectorUtils.removeMap(vector, key))
  def get(key: K): Option[V] = vector.find(_._1 == key).map(_._2)
  def iterator: Iterator[(K, V)] = vector.iterator

  def update[W >: V](p: (K, W)): VectorMap[K, W] = PlainVectorMap(update_vector(vector, p))
  def update[W >: V](p: Map[K, W]): VectorMap[K, W] = PlainVectorMap(
    update_vector(vector, p)
  )
  def remove(k: K): VectorMap[K, V] = this.-(k)
}
object PlainVectorMap {
  private val _empty = PlainVectorMap(Vector.empty)
  def empty[K, V] = _empty.asInstanceOf[PlainVectorMap[K, V]]
}

case class IndexedVectorMap[K, +V](
  vector: Vector[(K, V)],
  map: Map[K, V]
) extends VectorMap[K, V] {
  def ++[W >: V](p: VectorMap[K, W]): VectorMap[K, W] = IndexedVectorMap(
    vector ++ p.vector,
    map ++ p.vector.toMap
  )
  def +[W >: V](kv: (K, W)): VectorMap[K, W] =
    IndexedVectorMap(
      VectorUtils.updateMap(vector, kv),
      map + kv
    )
  def -(key: K): VectorMap[K, V] =
    IndexedVectorMap(
      VectorUtils.removeMap(vector, key),
      map - key
    )
  def get(key: K): Option[V] = map.get(key)
  def iterator: Iterator[(K, V)] = vector.iterator

  def update[W >: V](p: (K, W)): VectorMap[K, W] = IndexedVectorMap(
    update_vector(vector, p),
    map + p
  )
  def update[W >: V](p: Map[K, W]): VectorMap[K, W] = IndexedVectorMap(
    update_vector(vector, p),
    map ++ p
  )
  def remove(k: K): VectorMap[K, V] = this.-(k)
}
object IndexedVectorMap {
  private val _empty = IndexedVectorMap(Vector.empty, Map.empty)
  def empty[K, V] = _empty.asInstanceOf[IndexedVectorMap[K, V]]
}

object VectorMap {
  private val _empty = PlainVectorMap.empty
  def empty[K, V] = _empty.asInstanceOf[PlainVectorMap[K, V]]

  def apply[K, V](): VectorMap[K, V] = empty
  def apply[K, V](p: (K, V), ps: (K, V)*): VectorMap[K, V] = PlainVectorMap(p +: ps.toVector)
  def apply[K, V](ps: Iterable[(K, V)]): VectorMap[K, V] = PlainVectorMap(ps.toVector)

  implicit def VectorMapMonoid[K, V: Monoid] = new Monoid[VectorMap[K, V]] {
    def zero = empty
    def append(lhs: VectorMap[K, V], rhs: => VectorMap[K, V]) = {
      rhs.vector./:(lhs)((z, x) => z.get(x._1) match {
        case Some(s) =>
          val b = x._1 -> (s |+| x._2)
          z.update(b)
        case None => z.update(x)
      })
    }
  }
}
