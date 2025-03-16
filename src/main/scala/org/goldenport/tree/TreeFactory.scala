package org.goldenport.tree

import org.goldenport.RAISE

/*
 * @since   Nov. 15, 2020
 *  version Nov. 16, 2020
 * @version Mar.  4, 2025
 * @author  ASAMI, Tomoharu
 */
trait TreeFactory[E] {
  def createTree(node: TreeNode[E]): Tree[E]
  def createTreeNode(name: String, content: E, children: Seq[TreeNode[E]]): TreeNode[E]
  def createTreeNode(name: String, content: Option[E], children: Seq[TreeNode[E]]): TreeNode[E]

  def cloneTreeNodeDescendants(p: TreeNode[E]): TreeNode[E] = {
    val children = p.children.map(cloneTreeNodeDescendants)
    createTreeNode(p.name, p.content, children)
  }

  def cloneTreeNodeDescendants(name: String, p: TreeNode[E]): TreeNode[E] = {
    val children = p.children.map(cloneTreeNodeDescendants)
    createTreeNode(name, p.content, children)
  }
}

object TreeFactory {
  private val _default = new PlainTree.PlainTreeFactory()
  def default[E] = _default.asInstanceOf[TreeFactory[E]]
}
