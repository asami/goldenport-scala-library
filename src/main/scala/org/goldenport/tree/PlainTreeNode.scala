package org.goldenport.tree

import org.goldenport.context.Showable
import org.goldenport.values.CompactUuid
import org.goldenport.util.AnyUtils

/*
 * @since   Jul. 28, 2008
 *          Aug. 26, 2008
 *  version Nov. 18, 2019
 *  version Nov. 15, 2020
 *  version Dec. 26, 2020
 * @version Mar.  6, 2025
 * @author  ASAMI, Tomoharu
 */
class PlainTreeNode[E](aName: String) extends TreeNodeBase[E] with Showable.Control {
  type TreeNode_TYPE = PlainTreeNode[E]
  set_name(aName)

  def this() = this(PlainTreeNode.generate_anonymous_name)

  protected def show_Name: String = s"TreeNode($name)"
  override protected def print_String: String = Option(content).fold("")(to_print)
  override protected def display_String: String = Option(content).fold("")(to_display)
  protected def show_String: String = Option(content).fold("")(to_show)

  def new_Node(name: String): TreeNode_TYPE = {
    new PlainTreeNode[E](name)
  }
}

object PlainTreeNode {
  def create[E](name: String, children: Seq[TreeNode[E]]): PlainTreeNode[E] = {
    val r = new PlainTreeNode[E](name)
    r.addChildren(children)
    r
  }

  def create[E](name: String, content: E, children: Seq[TreeNode[E]]): PlainTreeNode[E] = {
    val r = new PlainTreeNode[E](name)
    r.content = content
    r.addChildren(children)
    r
  }

  def createContentNode[E](content: E, children: Seq[TreeNode[E]]): PlainTreeNode[E] =
    create(generate_anonymous_name, content, children)

  protected final def generate_anonymous_name = CompactUuid.generateString
}
