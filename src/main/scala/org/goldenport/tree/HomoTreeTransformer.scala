package org.goldenport.tree

import org.goldenport.RAISE

/*
 * @since   Mar.  7, 2025
 * @version Mar.  7, 2025
 * @author  ASAMI, Tomoharu
 */
trait HomoTreeTransformer[A] extends TreeTransformer[A, A] {
  override def isEndomap = true
}
