package org.goldenport.tree

/*
 * @since   Jul. 27, 2008
 *          Aug. 12, 2008
 *  version Nov. 18, 2019
 *  version Nov. 15, 2020
 *  version Feb.  2, 2021
 * @version Dec. 29, 2022
 * @author  ASAMI, Tomoharu
 */
class PlainTree[E](node: TreeNode[E]) extends TreeBase[E] {
  type TreeNode_TYPE = TreeNode[E]
  require(node != null)
  set_root(node)

  def this() = this(new PlainTreeNode[E]())

  override protected def copy_Node(aSource: TreeNode[E], aTarget: TreeNode[E]) {
    aTarget match {
      case m: TreeNodeBase[_] => m.content = aSource.content
      case m => super.copy_Node(aTarget, aTarget)
    }
  }
}

object PlainTree {
  class PlainTreeFactory[E]() extends TreeFactory[E] {
    def createTree(node: TreeNode[E]): Tree[E] = new PlainTree(node)

    def createTreeNode(name: String, content: E, children: Seq[TreeNode[E]]): TreeNode[E] =
      PlainTreeNode.create(name, content, children)
  }

  def create[E](root: E): PlainTree[E] = new PlainTree(PlainTreeNode.create("", root, Nil))

  def createFactory[E](): PlainTreeFactory[E] = new PlainTreeFactory[E]()
}
