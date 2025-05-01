package org.goldenport.tree

import scala.xml.Node
import scalaz.{Tree => ZTree}
import org.goldenport.RAISE
import org.goldenport.extension.Showable
import org.goldenport.values.PathName

/*
 * @since   Jul. 27, 2008
 *  version Apr. 17, 2011
 *  version Feb. 21, 2012
 *  version Apr. 30, 2012
 *  version Nov.  2, 2012
 *  version Nov. 18, 2019
 *  version May.  4, 2020
 *  version Oct. 17, 2020
 *  version Nov. 16, 2020
 *  version Feb.  2, 2021
 *  version Mar. 19, 2022
 *  version Mar.  5, 2025
 * @version Apr. 23, 2025
 * @author  ASAMI, Tomoharu
 */
trait Tree[E] extends Showable {
  type TreeNode_TYPE <: TreeNode[E]

  def root: TreeNode_TYPE
  def isModified: Boolean
  def setModified(): Unit // Visibility
  def clearModified(): Unit // Visibility
  def getNode(pathname: String): Option[TreeNode_TYPE]
  def getContent(path: String): Option[E]
  def setNode(pathname: String): TreeNode_TYPE
  def setContent(pathname: PathName, data: E): TreeNode_TYPE
  def setContent(pathname: String, data: E): TreeNode_TYPE
  def copyIn(aSource: Tree[E]): Unit
  def traverse(visitor: TreeVisitor[E]): Unit
  def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean): Unit
  def traverse(aProcedure: E => Unit)
  def collect(aCollector: TreeNode[E] => Boolean): Seq[TreeNode[E]]
  //
  def collect[T](pf: PartialFunction[TreeNode[E], T]): Seq[T]
  def collect[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Seq[T]
  def traverse[T](pf: PartialFunction[TreeNode[E], T]): Unit
  def traverse[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Unit
  def collectContent[T](pf: PartialFunction[E, T]): Seq[T]
  def collectContent[T](pathname: String, pf: PartialFunction[E, T]): Seq[T]
  def traverseContent[T](pf: PartialFunction[E, T]): Unit
  def traverseContent[T](pathname: String, pf: PartialFunction[E, T]): Unit
  def transform[T](t: TreeTransformer[E, T]): Tree[T] = t.apply(this)
  //
  def cursor: TreeCursor[E]
  def toXml: Node
  def toPrettyXml: String
  def ztree: ZTree[TreeNode[E]]
  //
  def print: String = show
  def display: String = s"Tree"
  def show: String = {
    val printer = Printer.create[E]()
    traverse(printer)
    printer.print
  }
}

object Tree {
  def create[T](): Tree[T] = new PlainTree[T]()
  def create[T](root: T): Tree[T] = PlainTree.create(root)
  def create[T](root: TreeNode[T]): Tree[T] = new PlainTree(root)

  sealed trait MergeContentStrategy
  case object OverWriteMerge extends MergeContentStrategy
  case object ComplementMerge extends MergeContentStrategy
  case object MonoidMerge extends MergeContentStrategy
  case object LeftMerge extends MergeContentStrategy
  case object RightMerge extends MergeContentStrategy

  def mergeClone[E](lhs: Tree[E], pathname: String, rhs: Tree[E]): Tree[E] =
    mergeClone(ComplementMerge, lhs, pathname, rhs)

  def mergeClone[E](strategy: MergeContentStrategy, lhs: Tree[E], pathname: String, rhs: Tree[E]): Tree[E] =
    new Merger[E](strategy).apply(lhs, pathname, rhs)

  def mergeClone[E](lhs: Tree[E], rhs: Tree[E]): Tree[E] =
    mergeClone(ComplementMerge, lhs, rhs)

  def mergeClone[E](strategy: MergeContentStrategy, lhs: Tree[E], rhs: Tree[E]): Tree[E] =
    new Merger[E](strategy).apply(lhs, rhs)

  def mergeClone[E](lhs: TreeNode[E], pathname: String, rhs: TreeNode[E]): TreeNode[E] =
    new Merger[E](ComplementMerge).apply(lhs, pathname, rhs)

  class Merger[E](
    strategy: MergeContentStrategy,
    factory: TreeFactory[E] = TreeFactory.default[E]
  ) {
    def apply(lhs: Tree[E], rhs: Tree[E]): Tree[E] = {
      val root = apply(lhs.root, rhs.root)
      new PlainTree(root)
    }

    def apply(lhs: TreeNode[E], rhs: TreeNode[E]): TreeNode[E] = {
      case class Z(
        resolved: Vector[TreeNode[E]] = Vector.empty,
        unresolved: Vector[TreeNode[E]] = rhs.children.toVector
      ) {
        def r = resolved ++ unresolved

        def +(rhs: TreeNode[E]) = {
          val (l, r) = unresolved.span(_.name != rhs.name)
          r.headOption.map(x => _merge(rhs, x, l ++ r.tail)).getOrElse(_clone(rhs))
        }

        private def _merge(node: TreeNode[E], counterpart: TreeNode[E], remainder: Vector[TreeNode[E]]) =
          copy(resolved = resolved :+ apply(node, counterpart), remainder)

        private def _clone(node: TreeNode[E]) =
          copy(resolved = resolved :+ factory.cloneTreeNodeDescendants(node))
      }
      val xs = lhs.children./:(Z())(_+_).r
      val r = new PlainTreeNode[E](lhs.name)
      r.addChildren(xs)
      r
    }

    def apply(lhs: Tree[E], pathname: String, rhs: Tree[E]): Tree[E] = {
      val root = apply(lhs.root, pathname, rhs.root)
      factory.createTree(root)
    }

    def apply(lhs: TreeNode[E], pathname: String, rhs: TreeNode[E]): TreeNode[E] =
      _merge(lhs, PathName(pathname).components, rhs)

    private def _merge(lhs: TreeNode[E], pathname: List[String], rhs: TreeNode[E]) =
      pathname match {
        case Nil => apply(lhs, rhs)
        case x :: xs =>
          val cs = lhs.children.find(_.name == x).map { node =>
            _merge_child(lhs.children, node, xs, rhs)
          }.getOrElse {
            val a = factory.cloneTreeNodeDescendants(x, rhs)
            lhs.children :+ a
          }
          factory.createTreeNode(lhs.name, lhs.content, cs)
      }

    private def _merge_child(children: Seq[TreeNode[E]], node: TreeNode[E], pathname: List[String], rhs: TreeNode[E]): Seq[TreeNode[E]] =
      pathname match {
        case Nil => RAISE.noReachDefect
        case x :: Nil =>
          children.map { c =>
            if (c == node) {
              apply(c, rhs)
            } else {
              factory.cloneTreeNodeDescendants(c)
            }
          }
        case x :: xs =>
          children.map { c =>
            if (c == node) {
              factory.createTreeNode(c.name, c.content, _merge_child(c.children, node, xs, rhs))
            } else {
              factory.cloneTreeNodeDescendants(c)
            }
          }
      }

    //   Case class Z(
    //     pathcomps: List[String] = pn.components,
    //     resolved: Vector[TreeNode[E]] = Vector.empty,
    //     unresolved: Vector[TreeNode[E]] = rhs.children.toVector
    //   ) {
    //     def r = resolved ++ unresolved

    //     def +(rhs: TreeNode[E]) = {
    //       val (l, r) = unresolved.span(_.name != rhs.name)
    //       r.headOption.map(x => _merge(rhs, x, l ++ r.tail)).getOrElse(_clone(rhs))
    //     }

    //     private def _merge(node: TreeNode[E], counterpart: TreeNode[E], remainder: Vector[TreeNode[E]]) =
    //       copy(resolved = resolved :+ apply(node, counterpart), unresolved = remainder)

    //     private def _clone(node: TreeNode[E]) =
    //       copy(resolved = resolved :+ factory.cloneTreeNodeDescendants(node))
    //   }
    //   val xs = lhs.children./:(Z())(_+_).r
    //   val r = new PlainTreeNode[E](lhs.name)
    //   r.addChildren(xs)
    //   r
    // }
  }
}
