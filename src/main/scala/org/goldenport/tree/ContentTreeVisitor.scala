package org.goldenport.tree

/*
 * @since   Apr. 27, 2025
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait ContentTreeVisitor[E] extends TreeVisitor[E] {
  override final def start(node: TreeNode[E]): Unit = {
    start_Prologue(node)
    node.getContent.fold(start_Container(node))(start_Content(node, _))
    start_Epilogue(node)
  }

  protected def start_Container(node: TreeNode[E]): Unit = {}
  protected def start_Content(node: TreeNode[E], content: E): Unit = {}
  protected def start_Prologue(node: TreeNode[E]): Unit = {}
  protected def start_Epilogue(node: TreeNode[E]): Unit = {}


  override final def enter(node: TreeNode[E]): Unit = {
    enter_Prologue(node)
    node.getContent.fold(enter_Container(node))(enter_Content(node, _))
    enter_Epilogue(node)
  }

  protected def enter_Container(node: TreeNode[E]): Unit = {}
  protected def enter_Content(node: TreeNode[E], content: E): Unit = {}
  protected def enter_Prologue(node: TreeNode[E]): Unit = {}
  protected def enter_Epilogue(node: TreeNode[E]): Unit = {}

  override final def leave(node: TreeNode[E]): Unit = {
    leave_Prologue(node)
    node.getContent.fold(leave_Container(node))(leave_Content(node, _))
    leave_Epilogue(node)
  }

  protected def leave_Container(node: TreeNode[E]): Unit = {}
  protected def leave_Content(node: TreeNode[E], content: E): Unit = {}
  protected def leave_Prologue(node: TreeNode[E]): Unit = {}
  protected def leave_Epilogue(node: TreeNode[E]): Unit = {}

  override final def leaveEnd(node: TreeNode[E]): Unit = {
    leaveEnd_Prologue(node)
    node.getContent.fold(leaveEnd_Container(node))(leaveEnd_Content(node, _))
    leaveEnd_Epilogue(node)
  }

  protected def leaveEnd_Container(node: TreeNode[E]): Unit = {}
  protected def leaveEnd_Content(node: TreeNode[E], content: E): Unit = {}
  protected def leaveEnd_Prologue(node: TreeNode[E]): Unit = {}
  protected def leaveEnd_Epilogue(node: TreeNode[E]): Unit = {}
}
