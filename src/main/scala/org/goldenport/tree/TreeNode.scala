package org.goldenport.tree

import scala.xml.Node
import scalaz._
import Scalaz._
import org.goldenport.realm.Realm
import org.goldenport.values.PathName

/*
 * @since   Jul. 27, 2008
 *  version Apr. 17, 2011
 *  version Feb. 22, 2012
 *  version Nov. 18, 2019
 *  version Oct. 11, 2020
 * @version Nov. 15, 2020
 * @author  ASAMI, Tomoharu
 */
trait TreeNode[E] {
  type TreeNode_TYPE <: TreeNode[E]

  def name: String
  def title: String
  def title_=(s: String)
  def content: E
  def content_=(aContent: E)
  def parent: TreeNode_TYPE
  def parent_=(aParent: TreeNode[E]): Unit // visibility
  def facade: Tree[E]
  def facade_=(aFacade: Tree[E]): Unit // visibility
  def isModified: Boolean
  def setModified(): Unit // visibility
  def isRoot: Boolean
  def isContainer: Boolean
  def isLeaf: Boolean
  def isEmpty: Boolean
  def length: Int
  def children: Seq[TreeNode_TYPE]
  def indexOf(node: TreeNode[E]): Int
  def getChild(index: Int): TreeNode_TYPE
  def getChild(name: String): Option[TreeNode_TYPE]
  def setChild(name: String): TreeNode_TYPE
  def addChild(): TreeNode_TYPE
  def addChild(child: TreeNode[E]): TreeNode_TYPE
  def addChildren(parent: TreeNode[E]): Unit
  def addChildren(ps: Seq[TreeNode[E]]): Unit
  def addContent(content: E): TreeNode_TYPE
  def removeChild(child: TreeNode[E])
  def getNode(pathname: String): Option[TreeNode_TYPE]
  def getNode(pathname: PathName): Option[TreeNode_TYPE]
  def setNode(pathname: String): TreeNode_TYPE
  def setNode(pathname: PathName): TreeNode_TYPE
  def setContent(pathname: String, content: E): TreeNode_TYPE
  def setContent(pathname: PathName, content: E): TreeNode_TYPE
  def clear(): Unit
  def traverse(visitor: TreeVisitor[E])
  def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean)
  def traverseNode(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean)
  def traverseChildren(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean)
  def traverse(aFunction: E => Unit)
  def cursor: TreeCursor[E]
  def deepCopy: TreeNode_TYPE
  def toText: String
  def buildText(buffer: StringBuilder): StringBuilder
  def toXml: Node
  def toPrettyXml: String

  /*
   * XXX pathname format variation - absolute, container
   */
  def pathname: String = {
    val pb = new GPathnameBuffer
    pb.absolute = true
//    pb.container = isContainer
    var node: TreeNode[E] = this
    while (!node.isRoot) {
      pb.addContainer(node.name)
      node = node.parent
    }
    pb.toString
  }
}

object TreeNode {
  lazy val _empty = new PlainTreeNode()
  def empty[T <: Realm.Data](): TreeNode[T] = _empty.asInstanceOf[TreeNode[T]]

//  def cloneNode[T](p: TreeNode[T]): TreeNode[T] = ???
}

// class TreeNodeShow[E] extends Show[TreeNode[E]] {
//   def show(a: TreeNode[E]) = {
//     val d = for {
//       b <- Option(a)
//       c <- Option(b.name)
//     } yield c
//     (d | "-")
//   }
// }
