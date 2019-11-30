package org.goldenport.tree

import scala.collection.mutable.ListBuffer

/**
 * @since   May.  3, 2012
 * @version Nov. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class PartialFunctionTreeContentCollector[E, R](val pf: PartialFunction[E, R]) extends TreeVisitor[E] {
  val collection = new ListBuffer[R]
  override def startEnter(aNode: TreeNode[E]) {
    val c = aNode.content
    if (pf.isDefinedAt(c)) {
      collection += pf(c)
    }
  }

  def result: List[R] = collection.toList
}
