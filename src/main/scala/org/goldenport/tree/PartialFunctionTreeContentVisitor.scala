package org.goldenport.tree

/**
 * @since   May.  3, 2012
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class PartialFunctionTreeContentVisitor[E](val pf: PartialFunction[E, _]) extends TreeVisitor[E] {
  override def startEnter(node: TreeNode[E]): Unit = {
    val c = node.content
    if (pf.isDefinedAt(c)) {
      pf(c)
    }
  }
}
