package org.goldenport.tree

import scala.xml.Node
import scalaz.{Tree => ZTree, _}
import Scalaz._
import org.goldenport.extension.Showable
import org.goldenport.values.PathName

/*
 * @since   Aug. 12, 2008
 *  version Apr. 17, 2011
 *  version Feb. 27, 2012
 *  version May.  6, 2012
 *  version Nov.  2, 2012
 *  version Nov. 18, 2019
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait TreeBase[E] extends Tree[E] with Showable {
  private var root_node: TreeNode_TYPE = _
  private var is_modified: Boolean = false

  def print: String = ???
  def display: String = ???
  def show: String = ???
  def embed: String = ???

  protected def dbc_invariants {
    assert (root_node != null, "Tree not opened")
  }

  protected final def set_root(aRoot: TreeNode_TYPE) {
    require (aRoot != null)
    root_node = aRoot
    root_node.facade = this
  }

  protected final def set_root {
    set_root(new PlainTreeNode[E].asInstanceOf[TreeNode_TYPE])
  }

  final def root: TreeNode_TYPE = {
    dbc_invariants
    return root_node
  }

  def isModified: Boolean = is_modified

  final def setModified() { // Visibility
    is_modified = true
    set_Modified()
  }

  def set_Modified(): Unit = Unit

  final def clearModified() { is_modified = false } // Visibility

  final def getNode(path: String): Option[TreeNode_TYPE] = {
    require(path != null)
    dbc_invariants
    root_node.getNode(path).asInstanceOf[Option[TreeNode_TYPE]]
  }

  final def getContent(path: String): Option[E] = {
    require(path != null)
    dbc_invariants
    val mayNode = root_node.getNode(path)
    if (mayNode.isEmpty) None
    else if (mayNode.get.content != null) Some(mayNode.get.content)
    else None
  }

  final def setNode(path: String): TreeNode_TYPE = {
    require(path != null)
    dbc_invariants
    root_node.setNode(path).asInstanceOf[TreeNode_TYPE]
  }

  final def setContent(path: String, content: E): TreeNode_TYPE = {
    require(path != null)
    dbc_invariants
    root_node.setContent(path, content).asInstanceOf[TreeNode_TYPE]
  }

  final def setContent(path: PathName, content: E): TreeNode_TYPE = {
    require(path != null)
    dbc_invariants
    root_node.setContent(path, content).asInstanceOf[TreeNode_TYPE]
  }

/*
  final def setContent(path: String, content: E): E = {
    require(path != null)
    assert(root_node != null)
    root_node.setNode(path, content)
    content
  }
*/

  final def copyIn(aSource: Tree[E]) {
    require(aSource != null)
    dbc_invariants
    open_Source(aSource)
    copy_in(aSource.root, root)
    close_Source(aSource)
  }

  final def copyIn(aPathname: String, aSource: Tree[E]) {
    require(aSource != null)
    dbc_invariants
    open_Source(aSource)
    copy_in(aSource.root, setNode(aPathname))
    close_Source(aSource)
  }

  private def copy_in(aSource: TreeNode[E], aTarget: TreeNode[E]) {
    for (sourceChild <- aSource.children) {
      val targetChild = aTarget.setChild(sourceChild.name)
      copy_Node(sourceChild, targetChild)
      copy_in(sourceChild, targetChild)
    }
  }

  protected def copy_Node(aSource: TreeNode[E], aTarget: TreeNode[E]) {
    sys.error("missing implementation copy_Node(TreeNode[E], TreeNode[E]) : " + this)
  }

  protected def open_Source(aSource: Tree[E]): Unit = Unit

  protected def close_Source(aSource: Tree[E]): Unit = Unit

  final def traverse(visitor: TreeVisitor[E]) {
    dbc_invariants
    root_node.traverse(visitor)
  }

  final def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean) {
    dbc_invariants
    root_node.traverse(visitor, filter)
  }

  final def traverse(aFunction: E => Unit) {
    traverse(new FunctionVisitor[E](aFunction))
  }

  final def collect(aFilter: TreeNode[E] => Boolean): Seq[TreeNode[E]] = {
    val collector = new TreeCollector[E](aFilter)
    traverse(collector)
    collector.result
  }

  final def collect[T](pf: PartialFunction[TreeNode[E], T]): Seq[T] = {
    sys.error("not implemented yet")
  }

  final def collect[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Seq[T] = {
    sys.error("not implemented yet")
    
  }

  final def traverse[T](pf: PartialFunction[TreeNode[E], T]): Unit = {
    sys.error("not implemented yet")
    
  }

  final def traverse[T](pathname: String, pf: PartialFunction[TreeNode[E], T]): Unit = {
    sys.error("not implemented yet")
    
  }

  final def collectContent[T](pf: PartialFunction[E, T]): Seq[T] = {
    val collector = new PartialFunctionTreeContentCollector(pf)
    traverse(collector)
    collector.result
  }

  final def collectContent[T](pathname: String, pf: PartialFunction[E, T]): Seq[T] = {
    sys.error("not implemented yet")
    
  }

  final def traverseContent[T](pf: PartialFunction[E, T]): Unit = {
    val visitor = new PartialFunctionTreeContentVisitor(pf)
    traverse(visitor)
  }

  final def traverseContent[T](pathname: String, pf: PartialFunction[E, T]): Unit = {
    sys.error("not implemented yet")
    
  }

  final def cursor: TreeCursor[E] = {
    new TreeCursor[E](this)
  }

  final def toXml: Node = {
    dbc_invariants
    root_node.toXml
  }

  final def toPrettyXml: String = {
    dbc_invariants
    root_node.toPrettyXml
  }

  def ztree: ZTree[TreeNode[E]] = {
    _ztree(root_node)
  }

  private def _ztree(node: TreeNode[E]): ZTree[TreeNode[E]] = {
    val cs = node.children.toStream.map {
      x => _ztree(x.asInstanceOf[TreeNode[E]])
    }
    ZTree.node(node, cs)
  }

  def ztree0: ZTree[E] = {
    _ztree0(root_node)
  }

  private def _ztree0(node: TreeNode_TYPE): ZTree[E] = {
    val cs = node.children.toStream.map {
      x => _ztree0(x.asInstanceOf[TreeNode_TYPE])    
    }
    ZTree.node(node.content, cs)
  }

  /*
   * Debug
   */
  // override def dumpString(): String = {
  //   val buf = new StringBuilder
  //   traverse(new TreeVisitor[E] {
  //     override def startEnter(node: TreeNode[E]) {
  //   	buf.append(node.pathname)
  //       buf.append("\n")
  //     }
  //   })
  //   buf.toString
  // }

  // override def dump(): Unit = {
  //   traverse(new TreeVisitor[E] {
  //     override def startEnter(node: TreeNode[E]) {
  //   	println(node.pathname)
  //     }
  //   })
  // }
}
