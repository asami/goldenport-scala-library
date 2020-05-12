package org.goldenport.tree

import scala.xml.Node
import scalaz.{Tree => ZTree}
import org.goldenport.values.PathName

/*
 * @since   Jul. 27, 2008
 *  version Apr. 17, 2011
 *  version Feb. 21, 2012
 *  version Apr. 30, 2012
 *  version Nov.  2, 2012
 *  version Nov. 18, 2019
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait Tree[E] {
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
  def traverse(visitor: TreeVisitor[E])
  def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean)
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
  //
  def cursor: TreeCursor[E]
  def toXml: Node
  def toPrettyXml: String
  def ztree: ZTree[TreeNode[E]]
}
