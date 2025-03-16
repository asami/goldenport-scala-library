package org.goldenport.tree

/*
 * @since   Nov.  8, 2008
 *          Jan.  7, 2009
 *  version Nov. 18, 2019
 * @version Feb. 23, 2025
 * @author  ASAMI, Tomoharu
 */
class TreeCursor[E](aNode: TreeNode[E]) {
  private var _current: TreeNode[E] = aNode

  def this(aTree: Tree[E]) = this(aTree.root)

  final def enter(name: String) {
    _current = _current.setChild(name)
  }

  final def enterContent(content: E) {
    _current = _current.addContent(content)
  }

  final def leaveContent(content: E) {
    require (_current.content == content)
    _current = _current.parent
  }

  final def leave() {
    _current = _current.parent
  }

  final def set(name: String, content: E) {
    _current.setChild(name, content)
  }

  final def add(content: E) {
    _current.addContent(content)
  }

  final def node = _current
  final def content = _current.content
}
