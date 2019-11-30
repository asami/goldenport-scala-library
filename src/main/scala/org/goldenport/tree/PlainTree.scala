package org.goldenport.tree

/*
 * @since   Jul. 27, 2008
 *          Aug. 12, 2008
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class PlainTree[E](node: TreeNode[E]) extends TreeBase[E] {
  type TreeNode_TYPE = TreeNode[E]
  require(node != null)
  set_root(node)

  def this() = this(new PlainTreeNode[E]())
}
