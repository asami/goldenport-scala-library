package org.goldenport.tree

/*
 * @since   Jul. 28, 2008
 *          Aug. 26, 2008
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class PlainTreeNode[E](aName: String) extends TreeNodeBase[E] {
  type TreeNode_TYPE = PlainTreeNode[E]
  set_name(aName)

  def this() = this(null)

  def new_Node(name: String): TreeNode_TYPE = {
    new PlainTreeNode[E](name)
  }
}
