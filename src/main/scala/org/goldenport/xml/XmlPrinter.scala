package org.goldenport.xml

import scala.util.control.NonFatal
import scala.xml._
import org.goldenport.Strings
import org.goldenport.xml.XmlUtils.{escape, escapeAttrQuot}
import org.goldenport.value._
import org.goldenport.util.AnyUtils

/*
 * @since   Nov.  2, 2017
 *  version Nov. 15, 2017
 *  version Jan.  5, 2018
 *  version Mar.  6, 2018
 *  version Aug.  5, 2018
 * @version Mar. 29, 2025
 * @author  ASAMI, Tomoharu
 */
class XmlPrinter(
  val isEmptyTag: Boolean = true,
  val isEmptyTagSpace: Boolean = true,
  val nonEmptyTags: Set[String] = Set.empty,
  val emptyTags: Set[String] = Set.empty,
  val doctype: Option[String] = None,
  val inlineTags: Set[String] = Set.empty,
  val formattingMode: XmlPrinter.FormattingMode = XmlPrinter.FormattingMode.Pretty,
  val space: Int = 2
) {
  import XmlPrinter._

  private val _buffer = new StringBuilder()
  private var _indent = 0
  private var _stack: List[State] = Nil

  private def _newline = "\n"

  private def _p(c: Character): Unit = _buffer.append(c)
  private def _p(s: String): Unit = _buffer.append(s)
  private def _ps: Unit = _buffer.append(' ')
  private def _pl(s: String): Unit = {
    _p(s)
    _buffer.append(_newline)
  }
  private def _nl: Unit = _buffer.append(_newline)

  private def _up: Unit = _stack match {
    case Nil => // do nothing
    case _ => _indent = _indent + space
  }

  private def _down: Unit = _stack match {
    case Nil => // do nothing
    case _ => _indent = _indent - space
  }

  private def _print_indent: Unit = _buffer.append(" " * _indent)

  private def _up_print = {
    _up
    _print_indent
  }

  private def _down_print = {
    _down
    _print_indent
  }

  private def _in_block = {
    _stack = State.Block :: _stack
  }

  private def _in_inline = {
    _stack = State.Inline :: _stack
  }

  private def _out = {
    _stack = _stack.tail
  }

  def text = _buffer.toString

  private def _is_empty_tag(p: String): Boolean = {
    val t = p.toLowerCase
    (isEmptyTag && !nonEmptyTags.contains(t)) || emptyTags.contains(t)
  }

  def apply(p: NodeSeq): String = {
    _doctype_html(p)
    _node_seq(p)
    text
  }

  private def _doctype_html(p: NodeSeq): Unit = p match {
    case elem: Elem =>
      if (elem.label.toLowerCase == "html")
        doctype.foreach(_pl)
    case _ => Unit
  }

  private def _node_seq(n: NodeSeq): Unit = n match {
    case Text(s) => _text(s)
    case m: Elem => _element(m)
    case Group(xs) => xs.foreach(_node_seq(_))
    case m: SpecialNode => _p(m.toString)
    case m => Unit // _p(m.toString)
  }

  private def _text(s: String): Unit = formattingMode match {
    case FormattingMode.AsIs => _p(escape(s))
    case FormattingMode.Pretty => if (_not_space(s)) _p(escape(s))
    case FormattingMode.Condense => if (_not_space(s)) _p(escape(s))
  }

  private def _not_space(s: String) = !_is_space(s)

  private def _is_space(s: String) = s.forall {
    case ' ' => true
    case '\t' => true
    case '\n' => true
    case '\r' => true
    case _ => false
  }

  private def _element(elem: Elem): Unit =
    if (_is_paragraph(elem))
      _element_paragraph(elem)
    else if (_is_inline(elem))
      _element_inline(elem)
    else if (_is_empty(elem))
      _element_block_empty(elem)
    else
      _element_block(elem)

  private def _is_paragraph(elem: Elem): Boolean = elem.label.toLowerCase == "p"
  private def _is_inline(elem: Elem): Boolean = inlineTags.contains(elem.label.toLowerCase)

  private def _is_empty(elem: Elem): Boolean = elem.child.isEmpty

  private def _element_paragraph(elem: Elem): Unit = {
    _up_print
    _in_block
    _element_open(elem)
    _element_children(elem)
    _element_close(elem)
    _nl
    _down
    _out
  }

  private def _element_inline(elem: Elem): Unit = {
    _in_inline
    _element_open(elem)
    _element_children(elem)
    _element_close(elem)
    _out
  }

  private def _element_block(elem: Elem): Unit = {
    _up_print
    _in_block
    _element_open(elem)
    _nl
    _element_children(elem)
    _print_indent
    _element_close(elem)
    _nl
    _down
    _out
  }

  private def _element_block_empty(elem: Elem): Unit = {
    _up_print
    _in_block
    _element_open(elem)
    _element_close(elem)
    _nl
    _down
    _out
  }

  private def _element_open(elem: Elem): Unit = {
    val prefix = Option(elem.prefix)
    val label = elem.label
    val attrs = elem.attributes
    val scope = elem.scope
    val children = elem.child
    val tagname = prefix.fold(label)(x => s"${x}:${label}")
    _p('<')
    _p(tagname)
    if (attrs != Null) {
      _attributes(attrs)
    }
    if (!_is_empty_tag(elem))
      _p('>')
  }

  private def _element_children(elem: Elem): Unit = {
    val children = elem.child
    children.foreach(_node_seq)
  }

  private def _element_close(elem: Elem): Unit = {
    val prefix = Option(elem.prefix)
    val label = elem.label
    val tagname = prefix.fold(label)(x => s"${x}:${label}")
    if (_is_empty_tag(elem)) {
      if (isEmptyTagSpace)
        _ps
      _p("/>")
    } else {
      _p("</")
      _p(tagname)
      _p('>')
    }
  }

  private def _is_empty_tag(elem: Elem): Boolean = {
    val label = elem.label
    val children = elem.child
    children.isEmpty && _is_empty_tag(label)
  }

  @annotation.tailrec
  private def _attributes(attrs: MetaData): Unit = {
    if (attrs != Null) {
      _ps
      _p(attrs.key)
      _p("=\"")
      _p(escapeAttrQuot(attrs.value.map(_.text).mkString))
      _p('"')
      _attributes(attrs.next)
    }
  }
}

object XmlPrinter {
  sealed trait FormattingMode extends NamedValueInstance {
  }
  object FormattingMode extends EnumerationClass[FormattingMode] {
    val elements = Vector(AsIs, Pretty, Condense)

    case object AsIs extends FormattingMode {
      val name = "asis"
    }
    case object Pretty extends FormattingMode {
      val name = "pretty"
    }
    case object Condense extends FormattingMode {
      val name = "condense"
    }
  }

  sealed trait State
  object State {
    case object Block extends State
    case object Inline extends State
  }

  def html = new XmlPrinter(
    true,
    true,
    Set("script", "title", "i", "b", "span", "div"),
    Set("meta", "link"),
    Some("<!DOCTYPE html>"),
    Set( "i", "b", "span")
  )
  def htmlFragment = new XmlPrinter(
    true,
    true,
    Set("script", "title", "i", "b", "span", "div"),
    Set("meta", "link"),
    None,
    Set( "i", "b", "span")
  )
}
