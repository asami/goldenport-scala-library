package org.goldenport.tree

/*
 * @since   Nov.  8, 2008
 *          Jan.  7, 2009
 *  version Nov. 18, 2019
 *  version Feb. 23, 2025
 * @version Apr. 23, 2025
 * @author  ASAMI, Tomoharu
 */
class TreeCursor[E](aNode: TreeNode[E]) {
  private var _current: TreeNode[E] = aNode

  def this(aTree: Tree[E]) = this(aTree.root)

  def enter(name: String): TreeCursor[E] = {
    _current = _current.setChild(name)
    this
  }

  def enterContent(content: E): TreeCursor[E] = {
    _current = _current.addContent(content)
    this
  }

  def leaveContent(content: E): TreeCursor[E] = {
    require (_current.content == content)
    _current = _current.parent
    this
  }

  def leave(): TreeCursor[E] = {
    _current = _current.parent
    this
  }

  def set(name: String, content: E): TreeCursor[E] = {
    _current.setChild(name, content)
    this
  }

  def add(content: E): TreeCursor[E] = {
    _current.addContent(content)
    this
  }

  final def node = _current
  final def content = _current.content
}
