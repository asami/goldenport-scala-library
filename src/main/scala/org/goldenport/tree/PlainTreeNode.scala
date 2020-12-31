package org.goldenport.tree

import org.goldenport.values.CompactUuid

/*
 * @since   Jul. 28, 2008
 *          Aug. 26, 2008
 *  version Nov. 18, 2019
 *  version Nov. 15, 2020
 * @version Dec. 26, 2020
 * @author  ASAMI, Tomoharu
 */
class PlainTreeNode[E](aName: String) extends TreeNodeBase[E] {
  type TreeNode_TYPE = PlainTreeNode[E]
  set_name(aName)

  def this() = this(CompactUuid.generateString)

  def new_Node(name: String): TreeNode_TYPE = {
    new PlainTreeNode[E](name)
  }
}

object PlainTreeNode {
  def create[E](name: String, content: E, children: Seq[TreeNode[E]]): PlainTreeNode[E] = {
    val r = new PlainTreeNode[E](name)
    r.content = content
    r.addChildren(children)
    r
  }
}
