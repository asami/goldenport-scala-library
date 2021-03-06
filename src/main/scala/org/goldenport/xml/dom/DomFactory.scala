package org.goldenport.xml.dom

import scala.util.control.NonFatal
import org.w3c.dom._
import javax.xml.parsers._
import org.goldenport.Strings
import org.goldenport.xml.{XmlAttributes, XmlAttribute}
import org.goldenport.xml.dom.DomUtils.NS_HTML

/*
 * @since   Nov.  8, 2020
 *  version Nov. 29, 2020
 *  version Dec. 27, 2020
 * @version Feb.  4, 2021
 * @author  ASAMI, Tomoharu
 */
class DomFactory(
  val document: Document,
  val namespace: Option[String]
) {
  def empty(): DocumentFragment = document.createDocumentFragment()

  def element(name: String): Element = {
    require (Strings.notblankp(name), "name should not be blank")
//    assert (name != "fragment", "debug")
    try {
      namespace.map(ns =>
        document.createElementNS(ns, name)
      ).getOrElse(
        document.createElement(name)
      )
    } catch {
      case NonFatal(e) => throw new DomElementException(name, e)
    }
  }

  def element(name: String, attrs: XmlAttributes): Element = {
    val r = element(name)
    for (a <- attrs.attributes) {
      r.setAttribute(a.qName, a.value)
    }
    r
  }

  def element(name: String, attrs: Iterable[(String, String)]): Element = {
    val r = element(name)
    attrs.foreach {
      case (k, v) => r.setAttribute(k, v)
    }
    r
  }

  def element(name: String, attrs: Iterable[(String, String)], child: Node): Element = {
    val r = element(name)
    attrs.foreach {
      case (k, v) => r.setAttribute(k, v)
    }
    append(r, child)
    r
  }

  def element(name: String, attrs: Iterable[(String, String)], children: Seq[Node]): Element = {
    val r = element(name)
    attrs.foreach {
      case (k, v) => r.setAttribute(k, v)
    }
    append(r, children)
    r
  }

  def element(name: String, node: Node): Element = {
    val r = element(name)
    append(r, node)
    r
  }

  def element(name: String, nodes: Seq[Node]): Element = {
    val r = element(name)
    append(r, nodes)
    r
  }

  def element(name: String, s: String): Element = {
    val r = element(name)
    append(r, text(s))
    r
  }

  def text(p: String): Text = document.createTextNode(p)

  def comment(p: String): Comment = document.createComment(p)

  def fragment(nodes: Seq[Node]): DocumentFragment = {
    val r = document.createDocumentFragment()
    append(r, nodes)
    r
  }

  def append(node: Node, child: Node, children: Node*): Node =
    DomUtils.append(node, child +: children)

  def append(node: Node, children: Seq[Node]): Node =
    DomUtils.append(node, children)

  def appendAttributes(node: Node, attrs: XmlAttributes): Node =
    ???

  def overwriteAttributes(node: Node, attrs: XmlAttributes): Node =
    ???
}

object DomFactory {
  def create(): DomFactory = {
    val factory = DocumentBuilderFactory.newInstance()
    val builder = factory.newDocumentBuilder()
    val doc = builder.newDocument()
    new DomFactory(doc, None)
  }

  def create(ns: String): DomFactory = {
    val factory = DocumentBuilderFactory.newInstance()
    val builder = factory.newDocumentBuilder()
    val doc = builder.newDocument()
    // TODO namespapce
    new DomFactory(doc, Some(ns))
  }

  def create(doc: Document): DomFactory = new DomFactory(doc, None)

  def create(doc: Document, ns: String): DomFactory = new DomFactory(doc, Some(ns))

  def createHtml(): DomFactory = create(NS_HTML)
}
