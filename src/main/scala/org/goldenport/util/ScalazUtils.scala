package org.goldenport.util

import scalaz._, Scalaz._

/*
 * @since   Sep. 12, 2015
 *  version Jul.  6, 2016
 *  version Sep. 22, 2016
 *  version Oct. 19, 2016
 *  version Jul. 25, 2017
 *  version Aug. 29, 2017
 *  version Jan. 20, 2018
 * @version Oct. 15, 2018
 * @version Dec.  7, 2018
 * @author  ASAMI, Tomoharu
 */
object ScalazUtils {
  def makeOptionNonEmptyList[T](a: Seq[T]): Option[NonEmptyList[T]] =
    a.toList match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, IList.fromList(xs)))
    }

  def makeOptionNonEmptyList[T](a: Option[T]): Option[NonEmptyList[T]] =
    a.map(NonEmptyList.nels(_))

  def makeSomeNonEmptyList[T](a: T): Option[NonEmptyList[T]] =
    Some(NonEmptyList.nels(a))

  // pre-order traversal
  def toIterator[T](tree: Tree[T]) = new TreeIterator(tree)

  class TreeIterator[T](tree: Tree[T]) extends Iterator[T] {
    private var _state: TreeIterator.IteratorState[T] =
      new TreeIterator.RootState(tree)

    def hasNext: Boolean = _state.hasNext
    def next(): T = {
      val (state, value) = _state.next()
      _state = state
      value
    }
  }

  object TreeIterator {
    sealed trait IteratorState[T] {
      def hasNext: Boolean
      def next(): (IteratorState[T], T)
      def isContinue: Boolean
    }

    class RootState[T](tree: Tree[T]) extends IteratorState[T] {
      def hasNext: Boolean = true
      def next(): (IteratorState[T], T) =
        (new ChildrenIteratorState(tree, this), tree.rootLabel)
      def isContinue = false
    }

    class ChildrenIteratorState[T](tree: Tree[T], parent: IteratorState[T]) extends IteratorState[T] {
      private var _children: Stream[Tree[T]] = tree.subForest

      def hasNext: Boolean = _children.nonEmpty || parent.isContinue
      def next(): (IteratorState[T], T) = _children match {
        case Stream.Empty => parent.next()
        case x #:: xs =>
          _children = xs
          (new ChildrenIteratorState(x, this), x.rootLabel)
      }
      def isContinue = _children.nonEmpty || parent.isContinue
    }
  }

  def excludeChildren[T](f: T => Boolean)(tree: Tree[T]): Tree[T] =
    Tree.node(tree.rootLabel, tree.subForest.flatMap(x =>
      if (f(x.rootLabel)) None else Some(excludeChildren(f)(x))
    ))

  def leafIterator[T](p: Tree[T]): Iterator[T] = {
    if (isLeaf(p))
      Iterator(p.rootLabel)
    else
      p.subForest.flatMap(leafIterator).toIterator
  }

  def isLeaf[T](p: Tree[T]): Boolean = p.subForest.isEmpty

  def toVector[T](p: Tree[T]): Vector[T] =
    p.rootLabel +: childrenToVector(p)

  def childrenToVector[T](p: Tree[T]): Vector[T] =
    p.subForest.toVector.flatMap(toVector)

  def toList[T](p: Tree[T]): List[T] = toVector(p).toList

  def toStream[T](p: Tree[T]): Stream[T] =
    p.rootLabel +: toStream(p)

  def childrenToStream[T](p: Tree[T]): Stream[T] = 
    p.subForest.flatMap(toStream)
}
