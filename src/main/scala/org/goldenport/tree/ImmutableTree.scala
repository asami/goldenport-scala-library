package org.goldenport.tree

import scala.xml.Node
import scalaz.{Tree => ZTree, _}
import Scalaz._
import org.goldenport.RAISE
import org.goldenport.values.PathName

/*
 * @since   May. 15, 2021
 * @version May. 17, 2021
 * @author  ASAMI, Tomoharu
 */
case class ImmutableTree[E](
  private val tree: Tree[E]
) extends Tree[E] {
  def root: TreeNode_TYPE = RAISE.notImplementedYetDefect
  def isModified: Boolean = RAISE.notImplementedYetDefect
  def setModified(): Unit = RAISE.notImplementedYetDefect // Visibility 
  def clearModified(): Unit = RAISE.notImplementedYetDefect // Visibility
  def getNode(pathname: String): Option[TreeNode_TYPE] = RAISE.notImplementedYetDefect
  def getContent(path: String): Option[E] = RAISE.notImplementedYetDefect
  def setNode(pathname: String): TreeNode_TYPE = RAISE.notImplementedYetDefect
  def setContent(pathname: PathName, data: E): TreeNode_TYPE = RAISE.notImplementedYetDefect
  def setContent(pathname: String, data: E): TreeNode_TYPE = RAISE.notImplementedYetDefect
  def copyIn(aSource: Tree[E]): Unit = RAISE.notImplementedYetDefect
  def traverse(visitor: TreeVisitor[E]): Unit = RAISE.notImplementedYetDefect
  def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean): Unit = RAISE.notImplementedYetDefect
  def traverse(aProcedure: E => Unit) = RAISE.notImplementedYetDefect
  def collect(aCollector: TreeNode[E] => Boolean): Seq[TreeNode[E]] = RAISE.notImplementedYetDefect
  //
  def collect[T](pf: PartialFunction[TreeNode[E], T]): Seq[T] = RAISE.notImplementedYetDefect
  def collect[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Seq[T] = RAISE.notImplementedYetDefect
  def traverse[T](pf: PartialFunction[TreeNode[E], T]): Unit = RAISE.notImplementedYetDefect
  def traverse[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Unit = RAISE.notImplementedYetDefect
  def collectContent[T](pf: PartialFunction[E, T]): Seq[T] = RAISE.notImplementedYetDefect
  def collectContent[T](pathname: String, pf: PartialFunction[E, T]): Seq[T] = RAISE.notImplementedYetDefect
  def traverseContent[T](pf: PartialFunction[E, T]): Unit = RAISE.notImplementedYetDefect
  def traverseContent[T](pathname: String, pf: PartialFunction[E, T]): Unit = RAISE.notImplementedYetDefect
  override def transform[T](t: TreeTransformer[E, T]): ImmutableTree[T] = RAISE.notImplementedYetDefect // t.apply(this)
  //
  def cursor: TreeCursor[E] = RAISE.notImplementedYetDefect
  def toXml: Node = RAISE.notImplementedYetDefect
  def toPrettyXml: String = RAISE.notImplementedYetDefect
  def ztree: ZTree[TreeNode[E]] = RAISE.notImplementedYetDefect
  //
  def merge(p: ImmutableTree[E]): ImmutableTree[E] = ImmutableTree(
    Tree.mergeClone(Tree.OverWriteMerge, tree, p.tree)
  )
}

object ImmutableTree {
  private val _empty = ImmutableTree[Any](Tree.create())
  def empty[E] = _empty.asInstanceOf[ImmutableTree[E]]
}
