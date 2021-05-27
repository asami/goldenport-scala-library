package org.goldenport.collection

import org.goldenport.RAISE
import org.goldenport.tree.{Tree, PlainTree, ImmutableTree}

/*
 * @since   May. 15, 2021
 * @version May. 23, 2021
 * @author  ASAMI, Tomoharu
 */
case class TreeMap[T](
  tree: ImmutableTree[T],
  map: Map[String, T],
  delimiter: String = "/"
) extends Map[String, T] {
  def +[S >: T](p: TreeMap[S]): TreeMap[S] =
    if (isEmpty)
      p
    else if (p.isEmpty)
      this.asInstanceOf[TreeMap[S]]
    else
      _add(p).asInstanceOf[TreeMap[S]]

  private def _add[S](p: TreeMap[S]): TreeMap[T] = {
    val a1 = p.tree.asInstanceOf[ImmutableTree[T]]
    val a2 = tree.merge(a1)
    val a = a2.asInstanceOf[ImmutableTree[T]]
    copy(
      tree = tree.merge(a),
      map = map ++ p.map.asInstanceOf[TreeMap[T]]
    )
  }

  def +[S >: T](kv: (String, S)): TreeMap[S] = RAISE.notImplementedYetDefect
  def -(key: String): TreeMap[T] = RAISE.notImplementedYetDefect
  def get(key: String): Option[T] = map.get(key)
  def iterator: Iterator[(String, T)] = map.iterator
}

object TreeMap {
  private val _empty =  TreeMap[Any](ImmutableTree.empty, Map.empty)
  def empty[E] = _empty.asInstanceOf[TreeMap[E]]

  def create[E](kv: (String, E), kvs: (String, E)*): TreeMap[E] = create(kv +: kvs)

  def create[E](ps: Seq[(String, E)]): TreeMap[E] = {
    val builder = Builder[E]()
    builder.add(ps)
    builder.build()
  }

  def create[E](delimiter: String, ps: Seq[(String, E)]): TreeMap[E] = {
    val builder = Builder[E](delimiter)
    builder.add(ps)
    builder.build()
  }

  case class Builder[T](
    delimiter: String = "/"
  ) {
    val tree = PlainTree.create[T](null.asInstanceOf[T])
    var map: Map[String, T] = Map.empty

    def add(ps: Seq[(String, T)]): Builder[T] = {
      ps.map {
        case (k, v) => add(k, v)
      }
      this
    }

    def add(name: String, value: T): Builder[T] = {
      // TODO tree
      map = map + (name -> value)
      this
    }

    def build(): TreeMap[T] = TreeMap(ImmutableTree(tree), map, delimiter)
  }
}
