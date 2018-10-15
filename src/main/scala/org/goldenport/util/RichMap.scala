package org.goldenport.util

/*
 * @since   Apr. 12, 2017
 * @version Sep.  1, 2017
 * @author  ASAMI, Tomoharu
 */
object RichMap {
  // Map.++ may not specify a key collision strategy.
  def update[K, V](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] = {
    case class Z(remainder: Map[K, V], result: Map[K, V] = Map.empty) {
      def r = result ++ remainder
      def +(kv: (K, V)) = {
        val (k, v) = kv
        remainder.get(k).fold(copy(result = result + (k -> v)))(x =>
          Z(remainder - k, result + (k -> x)))
      }
    }
    lhs./:(Z(rhs))(_+_).r
  }

  def update[K, V](lhs: Map[K, V], rhs: Map[K, V], ps: Map[K, V]*): Map[K, V] =
    ps./:(update(lhs, rhs))(update)

  def toStringListMap[K](pm: Map[K, _]): Map[K, List[String]] = {
    pm map {
      case (k, v) =>
        val v1 = v match {
          case m: String => List(m)
          case m: Seq[_] => m.toList.map(AnyUtils.toString)
          case m: Array[_] => m.toList.map(AnyUtils.toString)
          case m => List(m.toString)
        }
        k -> v1
    }
  }
}
