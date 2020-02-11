package org.goldenport.tree

/*
 * @since   Aug. 17, 2008
 *  version Apr. 17, 2011
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class FunctionVisitor[E](aFunction: E => Unit) extends TreeVisitor[E] {
  override def startEnter(node: TreeNode[E]): Unit = {
    if (node.content != null) aFunction(node.content)
  }
}
