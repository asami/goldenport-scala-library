package org.goldenport.tree

import scala.collection.mutable.ListBuffer

/*
 * @since   Sep.  6, 2008
 *  version Apr. 17, 2011
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class TreeCollector[E](val filter: TreeNode[E] => Boolean) extends TreeVisitor[E] {
  val collection = new ListBuffer[TreeNode[E]]
  override def startEnter(aNode: TreeNode[E]) {
    if (filter(aNode)) collection += aNode
  }

  def result: List[TreeNode[E]] = collection.toList
}
