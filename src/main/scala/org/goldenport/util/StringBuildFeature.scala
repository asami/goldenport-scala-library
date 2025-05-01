package org.goldenport.util

/*
 * @since   Apr. 27, 2025
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
trait StringBuildFeature {
  protected def sb_newline = "\n"
  protected def sb_indent_mark = " "
  protected def sb_indent_post_mark = ""
  protected def sb_indent_size: Int = 2

  private val _buffer = new StringBuilder()
  private var _indent_depth: Int = 0

  protected final def sb_indent_length = _indent_depth * sb_indent_size
  protected def sb_indent: String = sb_indent_mark * sb_indent_length + sb_indent_post_mark

  protected final def sb_to_string(): String = _buffer.toString()

  protected final def sb_enter(): Unit = _indent_depth = _indent_depth + 1

  protected final def sb_leave(): Unit = _indent_depth = _indent_depth - 1

  protected final def sb_print(p: String): Unit = {
    _buffer.append(p)
  }

  protected final def sb_println(): Unit = {
    _buffer.append(sb_newline)
  }

  protected final def sb_println(p: String): Unit = {
    _buffer.append(sb_indent)
    _buffer.append(p)
    _buffer.append(sb_newline)
  }

  protected final def sb_println_start(p: String): Unit = {
    _buffer.append(sb_indent)
    _buffer.append(p)
  }
}
