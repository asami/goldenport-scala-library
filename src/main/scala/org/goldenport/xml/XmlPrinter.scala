package org.goldenport.xml

import scala.util.control.NonFatal
import scala.annotation.tailrec
import scala.xml._
import org.goldenport.Strings
import org.goldenport.util.AnyUtils

/*
 * @since   Nov.  2, 2017
 * @version Nov.  2, 2017
 * @author  ASAMI, Tomoharu
 */
class XmlPrinter(
  val isEmptyTag: Boolean = true,
  val isEmptyTagSpace: Boolean = true,
  val nonEmptyTags: Set[String] = Set.empty,
  val emptyTags: Set[String] = Set.empty,
  val doctype: Option[String] = None
) {
  private val _buffer = new StringBuilder()

  private def _newline = "\n"

  private def p(c: Character): Unit = _buffer.append(c)
  private def p(s: String): Unit = _buffer.append(s)
  private def ps: Unit = _buffer.append(' ')
  private def pl(s: String): Unit = {
    _buffer.append(s)
    _buffer.append(_newline)
  }

  def text = _buffer.toString

  private def _is_empty_tag(p: String): Boolean = {
    val t = p.toLowerCase
    (isEmptyTag && !nonEmptyTags.contains(t)) || emptyTags.contains(t)
  }

  def apply(p: NodeSeq): String = {
    _doctype_html
    _node_seq(p)
    text
  }

  private def _doctype_html: Unit = doctype.foreach(pl)

  private def _node_seq(n: NodeSeq): Unit = n match {
    case Text(s) => p(s)
    case m: Elem => _element(m)
    case Group(xs) => xs.foreach(_node_seq(_))
    case m: SpecialNode => p(m.toString)
    case m => Unit // p(m.toString)
  }

  private def _element(elem: Elem): Unit = {
    val prefix = Option(elem.prefix)
    val label = elem.label
    val attrs = elem.attributes
    val scope = elem.scope
    val children = elem.child
    val tagname = prefix.fold(label)(x => s"${x}:${label}")
    p('<')
    p(tagname)
    if (attrs != Null) {
      _attributes(attrs)
    }
    if (children.isEmpty && _is_empty_tag(label)) {
      if (isEmptyTagSpace)
        ps
      p("/>")
    } else {
      p('>')
      children.foreach(_node_seq)
      p("</")
      p(tagname)
      p('>')
    }
  }

  @tailrec
  private def _attributes(attrs: MetaData): Unit = {
    if (attrs != Null) {
      ps
      p(attrs.key)
      p("=\"")
      p(attrs.value.map(_.text).mkString)
      p('"')
      _attributes(attrs.next)
    }
  }
}

object XmlPrinter {
  def html = new XmlPrinter(
    true,
    true,
    Set("script", "title"),
    Set("meta", "link"),
    Some("<!DOCTYPE html>")
  )
}
