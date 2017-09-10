package org.goldenport.xml

import scalaz.{Node =>_, _} , Scalaz._
import scala.util.control.NonFatal
import scala.annotation.tailrec
import scala.xml._
import org.goldenport.Strings

/*
 * @since   May. 25, 2014
 *  version Jun. 25, 2014
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object XmlUtils {
  def parseNode(p: String): Node = 
    if (p.startsWith("<"))
      XML.loadString(p)
    else
      Text(p)

  def parseNodeSeq(p: String): NodeSeq = 
    if (p.startsWith("<"))
      XML.loadString(p)
    else try {
      val s = s"<div>$p</div>"
      val x = XML.loadString(s)
      Group(x.child)
    } catch {
      case NonFatal(e) =>
        Text(p)
    }

  def makeString(p: String): String = try {
    parseNodeSeq(p).text
  } catch {
    case NonFatal(e) => p
  }

  def asString(node: Node): String = {
    node match {
      case Text(s) => s
      case PCData(s) => s
      case elem: Elem => elem.child.toVector.foldMap(asString)
    }
  }

  def asXmlString(node: Node): String = {
    node match {
      case Text(s) => s
      case PCData(s) => s
      case elem: Elem => _normalize(elem).toString
    }
  }

  def contentsAsXmlString(node: Node): String = {
    node.child.toVector.foldMap(asXmlString)
  }

  private def _normalize(elem: Elem): Elem = {
    elem.copy(
      scope = _normalize_namespace(_prefixes(elem), elem.scope))
  }

  private def _prefixes(node: Node): Set[String] = {
    node match {
      case elem: Elem => Set(elem.prefix) ++ elem.child.map(_prefixes).toVector.concatenate
      case _ => Set.empty
    }
  }

  private def _normalize_namespace(prefixes: Set[String], ns: NamespaceBinding): NamespaceBinding = {
    def build(parent: NamespaceBinding, prefix: String): NamespaceBinding = {
      if (isAlreadyDefined(parent, prefix)) parent
      else findNamespace(ns, prefix) match {
        case Some(s) => s.copy(parent = parent)
        case None => parent
      }
    }
    prefixes.foldLeft(TopScope: NamespaceBinding)(build)
  }

  @tailrec
  def isAlreadyDefined(ns: NamespaceBinding, prefix: String): Boolean = {
    ns match {
      case TopScope => false
      case _ if ns.prefix == prefix => true
      case _ => isAlreadyDefined(ns.parent, prefix)
    }
  }

  @tailrec
  def findNamespace(ns: NamespaceBinding, prefix: String): Option[NamespaceBinding] = {
    ns match {
      case TopScope => None
      case _ if ns.prefix == prefix => Some(ns)
      case _ => findNamespace(ns.parent, prefix)
    }
  }

  def findFirstElementByLabel(label: String, s: String): Option[Elem] = {
    (XML.loadString(s) \\ label).headOption.asInstanceOf[Option[Elem]]
  }

  @annotation.tailrec
  def isAttribute(attrs: MetaData, key: String): Boolean =
    if (attrs == null)
      false
    else
      attrs.key == key || isAttribute(attrs.next, key)

  @annotation.tailrec
  def isNonEmptyAttribute(attrs: MetaData, key: String): Boolean =
    if (attrs == Null)
      false
    else
      (attrs.key == key && isEmptyValue(attrs.value)) || isNonEmptyAttribute(attrs.next, key)

  def isEmptyValue(p: Seq[Node]): Boolean = p.toList match {
    case Nil => true
    case x :: Nil => x.text == ""
    case xs => false
  }

  def setAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    setOrRemoveAttribute(elem, key, value)

  def updateAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    setAttributeIfRequired(elem, key, value)

  def complementAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    complementAttributeIfRequired(elem, key, value)

  def setOrRemoveAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(removeAttribute(elem, key))(setAttribute(elem, key, _))

  def setAttributeIfRequired(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(elem)(setAttribute(elem, key, _))

  def complementAttributeIfRequired(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(elem)(complementAttribute(elem, key, _))

  def setAttribute(elem: Elem, key: String, value: String): Elem =
    elem.copy(attributes = setAttribute(elem.attributes, key, value))

  def setAttribute(attrs: MetaData, key: String, value: String): MetaData =
    if (attrs == Null)
      attrs
    else if (isAttribute(attrs, key)) {
      if (attrs.key == key)
        new UnprefixedAttribute(key, value, setAttribute(attrs.next, key, value))
      else
        attrs.copy(setAttribute(attrs.next, key, value))
    } else {
      new UnprefixedAttribute(key, value, attrs)
    }

  def complementAttribute(elem: Elem, key: String, value: String): Elem = 
    elem.copy(attributes = complementAttribute(elem.attributes, key, value))

  def complementAttribute(attrs: MetaData, key: String, value: String): MetaData =
    if (isAttribute(attrs, key))
      attrs
    else
      new UnprefixedAttribute(key, value, attrs)

  def removeAttribute(elem: Elem, key: String): Elem = 
    elem.copy(attributes = removeAttribute(elem.attributes, key))

  def removeAttribute(attrs: MetaData, key: String): MetaData =
    attrs.remove(key)

    def toSummary(s: String): String =
    if (Strings.blankp(s)) "" else {
      import org.cyberneko.html.parsers.DOMParser
      import org.xml.sax.InputSource
      import java.io.StringReader
      val parser = new DOMParser()
      val is = new InputSource(new StringReader(s))
      parser.parse(is)
      val dom = parser.getDocument()
      val text = _distill_text_dom(dom.getDocumentElement)
      Strings.cutstring(text, 200)
    }

  private def _distill_text_dom(elem: org.w3c.dom.Element): String = {
    val builder = new StringBuilder
    val a = elem.getElementsByTagName("body")
    if (a.getLength() == 0) {
      _distill_text(a.item(0), builder)
    } else {
      _distill_text(elem, builder)
    }
    builder.toString
  }

  private def _distill_text(n: org.w3c.dom.Node, builder: StringBuilder) {
    n match {
      case t: org.w3c.dom.Text => builder.append(t.getWholeText)
      case _ => {
        val xs = n.getChildNodes()
        for (i <- 0 until xs.getLength) {
          _distill_text(xs.item(i), builder)
        }
      }
    }
  }
}
