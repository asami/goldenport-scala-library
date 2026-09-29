package org.goldenport.collection

import org.goldenport.RAISE
import org.goldenport.tree.{Tree, PlainTree, ImmutableTree}

/*
 * @since   May. 15, 2021
 *  version May. 23, 2021
 * @version Sep. 30, 2026
 * @author  ASAMI, Tomoharu
 */
/**
 * A map keyed by path-like names. Keys are currently stored and looked up as
 * exact strings; delimiter does not yet split paths or enable subtree queries.
 *
 * The tree is reserved for future hierarchical operations (such as finding
 * entries beneath a path or hierarchical tags), but Builder does not populate
 * it yet. Adding or removing a single entry is also not implemented.
 */
case class PathMap[T](
  tree: ImmutableTree[T],
  map: Map[String, T],
  delimiter: String = "/"
) extends Map[String, T] {
  def +[S >: T](p: PathMap[S]): PathMap[S] =
    if (isEmpty)
      p
    else if (p.isEmpty)
      this.asInstanceOf[PathMap[S]]
    else
      _add(p).asInstanceOf[PathMap[S]]

  private def _add[S >: T](p: PathMap[S]): PathMap[S] = {
    val basetree = tree.asInstanceOf[ImmutableTree[S]]
    PathMap(basetree.merge(p.tree), map ++ p.map, delimiter)
  }

  def +[S >: T](kv: (String, S)): PathMap[S] = RAISE.notImplementedYetDefect
  def -(key: String): PathMap[T] = RAISE.notImplementedYetDefect
  def get(key: String): Option[T] = map.get(key)
  def iterator: Iterator[(String, T)] = map.iterator
}

object PathMap {
  private val _empty =  PathMap[Any](ImmutableTree.empty, Map.empty)
  def empty[E] = _empty.asInstanceOf[PathMap[E]]

  def create[E](kv: (String, E), kvs: (String, E)*): PathMap[E] = create(kv +: kvs)

  def create[E](ps: Seq[(String, E)]): PathMap[E] = {
    val builder = Builder[E]()
    builder.add(ps)
    builder.build()
  }

  def create[E](delimiter: String, ps: Seq[(String, E)]): PathMap[E] = {
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

    def build(): PathMap[T] = PathMap(ImmutableTree(tree), map, delimiter)
  }
}
