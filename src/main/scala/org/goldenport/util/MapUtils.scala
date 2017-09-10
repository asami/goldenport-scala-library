package org.goldenport.util

import scala.language.higherKinds
import scalaz._, Scalaz._
import scala.collection.generic.CanBuildFrom

/*
 * @since   Jun. 25, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object MapUtils {
  def toVectorMapViaChunks[K, V](
    f: Seq[K] => Map[K, Vector[V]],
    chunksize: Int
  )(ks: Seq[K]): Map[K, Vector[V]] = {
    case class Z(r: Map[K, Vector[V]] = Map.empty) {
      def +(rhs: Seq[K]) = {
        Z(r |+| f(rhs))
      }
    }
    ks.grouped(chunksize)./:(Z())(_+_).r
  }

  def toVectorMap[K, V](ps: Seq[(K, V)]): Map[K, Vector[V]] = {
    import scalaz._, Scalaz._
    case class Z(r: Map[K, Vector[V]] = Map.empty) {
      def +(rhs: (K, V)) = Z(r |+| Map(rhs._1 -> Vector(rhs._2)))
    }
    ps./:(Z())(_+_).r
  }

  def complement[K, V](master: Map[K, V], aux: Map[K, V]): Map[K, V] = {
    case class Z(r: Map[K, V]) {
      def +(rhs: (K, V)) = {
        val (k, v) = rhs
        if (r.contains(k))
          this
        else
          Z(r + rhs)
      }
    }
    aux./:(Z(master))(_+_).r
  }

  def complements[K, V](master: Map[K, V], auxs: Seq[Map[K, V]]): Map[K, V] =
    auxs./:(master)(complement)

  def complementT[K, V, M[_, _] <: Map[K, V]](master: M[K, V], aux: Map[K, V])(implicit bf: CanBuildFrom[Map[K, V], (K, V), M[K, V]]): M[K, V] = {
    case class Z(r: Map[K, V]) {
      def +(rhs: (K, V)) = {
        val (k, v) = rhs
        if (r.contains(k))
          this
        else
          Z(r + rhs)
      }
    }
    val a = aux./:(Z(master))(_+_).r
    bf(a).result()
  }

  def complementsT[K, V, M[_, _] <: Map[K, V]](master: M[K, V], auxs: Seq[Map[K, V]])(implicit bf: CanBuildFrom[Map[K, V], (K, V), M[K, V]]): M[K, V] =
    auxs./:(master)(complementT)
}
