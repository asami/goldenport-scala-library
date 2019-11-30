package org.goldenport.tree

import scala.collection.mutable.ArrayBuffer

/*
 * @since   Jul. 27, 2008
 *  version Apr. 17, 2011
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
trait TreeVisitor[E] {
  def startEnter(node: TreeNode[E]): Unit = Unit // XXX better name
  def start(node: TreeNode[E]): Unit = startEnter(node)
  def enter(node: TreeNode[E]): Unit = startEnter(node)
  def stay(node: TreeNode[E], index: Int, prev: TreeNode[E], next: TreeNode[E]): Unit = Unit
  def leave(node: TreeNode[E]): Unit = leaveEnd(node)
  def end(node: TreeNode[E]): Unit = leaveEnd(node)
  def leaveEnd(node: TreeNode[E]): Unit = Unit

  private var _done_nodes: ArrayBuffer[TreeNode[E]] = null

  private def done_nodes = {
    if (_done_nodes == null) _done_nodes = new ArrayBuffer[TreeNode[E]]
    _done_nodes
  }

  protected final def done_traverse(aNode: TreeNode[E]) {
    done_nodes += aNode
  }

  final def isDone(aNode: TreeNode[E]): Boolean = {
    if (_done_nodes == null) false
    else done_nodes.contains(aNode)
  }
}
