package org.goldenport.tree

import org.goldenport.context.Showable
import org.goldenport.util.AnyUtils

/*
 * @since   Nov.  7, 2020
 *  version Nov. 14, 2020
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
class Printer[E](
  tosf: E => String
) extends TreeVisitor[E] {
  private val _indent_width = 2
  private val _newline = "\n"
  private var _depth = 0

  private val _buffer = new StringBuilder()

  override def enter(node: TreeNode[E]): Unit = {
    val indent = Vector.fill(_depth * _indent_width)(' ').mkString
    _buffer.append(indent)
    val content = node.getContent.fold(node.name) { x =>
      s"${node.name}[${tosf(x)}]"
    }
    _buffer.append(content)
    _buffer.append(_newline)
    _depth = _depth + 1
  }

  override def leave(node: TreeNode[E]): Unit = {
    _depth = _depth - 1
  }

  def print: String = _buffer.toString
}

object Printer {
  def create[T](): Printer[T] = new Printer[T](Showable.toString(Showable.Kind.Display, _))
}
