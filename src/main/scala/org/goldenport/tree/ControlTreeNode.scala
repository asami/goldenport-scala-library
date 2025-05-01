package org.goldenport.tree

import org.goldenport.RAISE
import org.goldenport.context.Showable

/*
 * @since   Mar.  4, 2025
 * @version Mar.  6, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait ControlTreeNode[E] extends TreeNodeStructureBase[E] {
  def content: E = RAISE.unsupportedOperationFault
  def content_=(aContent: E): Unit = RAISE.unsupportedOperationFault
  def name: String = RAISE.unsupportedOperationFault
  protected def new_Node(name: String): TreeNode_TYPE = RAISE.unsupportedOperationFault
}

object ControlTreeNode {
  case class Empty[E]() extends ControlTreeNode[E] with Showable.Control {
    protected def show_Name: String = "TreeNode#Empty"
    protected def show_String: String = "Empty"
  }

  case class AsIs[E](node: TreeNode[E]) extends ControlTreeNode[E] {
    protected def show_Name: String = "TreeNode#AsIs"
    protected def show_String: String = node.embed
  }

  case class Collection[E](nodes: List[TreeNode[E]]) extends ControlTreeNode[E] {
    protected def show_Name: String = "TreeNode#Collection"
    protected def show_String: String = nodes.map(_.embed).mkString(",")
  }
}
