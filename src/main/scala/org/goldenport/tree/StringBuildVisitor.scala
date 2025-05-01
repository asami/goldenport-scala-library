package org.goldenport.tree

import scala.collection.mutable.ArrayBuffer
import org.goldenport.util.StringBuildFeature

/*
 * @since   Apr. 27, 2025
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait StringBuildVisitor[E] extends ContentTreeVisitor[E] with StringBuildFeature {
  override final def enter_Prologue(node: TreeNode[E]) {
    sb_enter()
  }

  override final def leave_Prologue(node: TreeNode[E]) {
    sb_leave()
  }
}
