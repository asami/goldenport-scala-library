package org.goldenport.xml

import scala.xml._
import org.xml.sax._
import org.xml.sax.helpers.XMLFilterImpl
import org.goldenport.Strings

/*
 * @since   Feb. 22, 2016
 *  version Apr. 18, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
class XmlReader(node: NodeSeq, autoDocument: Boolean = true) extends XMLFilterImpl {
  private lazy val _handler = getContentHandler

  override def setFeature(name: String, v: Boolean) {
//    println(s"feature: $name - $v")
  }

  override def setProperty(name: String, v: AnyRef) {
//    println(s"property: $name - $v")
  }

  override def parse(in: InputSource) {
    _traverse(TopScope, node)
  }

  private def _traverse(ns: NamespaceBinding, node: NodeSeq) {
    node match {
      case m: Document =>
        _handler.startDocument()
        _traverse_children(ns, m.children)
        _handler.endDocument()
      case m: Node if autoDocument =>
        _handler.startDocument()
        _traverse_node(ns, m)
        _handler.endDocument()
      case m: Node => _traverse_node(ns, m)
      case _ => ???
    }
  }

  private def _traverse_node(ns: NamespaceBinding, node: Node) {
    node match {
      case m: SpecialNode => _traverse_special(m)
      case m: Elem => _traverse_element(ns, m)
      case m: Group => _traverse_children(ns, m)
    }
  }

  private def _traverse_element(ns: NamespaceBinding, elm: Elem) {
    val newns = elm.scope
    val attrs = _attrs(ns, elm)
    // val (uri, localname, qname) = Strings.totokens(elm.label) match {
    //   case Nil => throw new IllegalArgumentException("Invalid attribute")
    //   case name :: Nil => Option(newns.getURI("")).fold(
    //     ("", name, name)
    //   ) { uri =>
    //     (uri, name, name)
    //   }
    //   case prefix :: name :: _ => Option(newns.getURI(prefix)).fold(
    //     ("", name, elm.label)
    //   ) { uri =>
    //     (uri, name, elm.label)
    //   }
    // }
    val localname = elm.label
    val prefix = Option(elm.prefix) getOrElse ""
    val uri = newns.getURI(prefix)
    val qname = prefix match {
      case "" => localname
      case m => s"$prefix:$localname"
    }
    _start_namespace_mapping(newns, ns)
    _handler.startElement(uri, localname, qname, attrs)
    _traverse_children(newns, elm)
    _handler.endElement(uri, localname, qname)
    _end_namespace_mapping(newns, ns)
  }

  // private def _traverse_element0(ns: NamespaceBinding, elm: Elem) {
  //   val (newns, attrs) = _ns_attrs(ns, elm)
  //   val (uri, localname, qname) = Strings.totokens(elm.label) match {
  //     case Nil => throw new IllegalArgumentException("Invalid attribute")
  //     case name :: Nil => Option(ns.getURI("")).fold(
  //       ("", name, name)
  //     ) { uri =>
  //       (uri, name, name)
  //     }
  //     case prefix :: name :: _ => Option(ns.getURI(prefix)).fold(
  //       ("", name, elm.label)
  //     ) { uri =>
  //       (uri, name, elm.label)
  //     }
  //   }
  //   _start_namespace_mapping(newns, ns)
  //   _handler.startElement(uri, localname, qname, attrs)
  //   _traverse_children(newns, elm)
  //   _handler.endElement(uri, localname, qname)
  //   _end_namespace_mapping(newns, ns)
  // }

  private def _attrs(basens: NamespaceBinding, elm: Elem): Attributes =
    _attrs(basens, elm.scope, elm.attributes, XmlAttributes.empty)

  private def _attrs(
    basens: NamespaceBinding,
    currentns: NamespaceBinding,
    attr: MetaData,
    attributes: XmlAttributes
  ): Attributes = {
    // val nsattrs = _ns_declare(basens, currentns, attributes)
    _attrs(currentns, attr, attributes)
  }

  private def _ns_declare(
    base: NamespaceBinding,
    current: NamespaceBinding,
    attributes: XmlAttributes
  ): XmlAttributes = {
    if (current != base && current != TopScope) {
      val a = Option(current.prefix).flatMap(x => if (x == "") None else Some(x)) match {
        case Some(s) =>
          val localname = s
          val prefix = "xmlns"
          val qname = s"$prefix:$localname"
          val value = current.uri
          XmlAttribute(localname, qname, Some(prefix), None, value)
        case None => 
          val localname = "xmlns"
          val qname = localname
          val value = current.uri
          XmlAttribute(localname, qname, None, None, value)
      }
      val b = attributes + a
      _ns_declare(base, current.parent, b)
    } else {
      attributes
    }
  }

  private def _attrs(
    ns: NamespaceBinding,
    attr: MetaData,
    attributes: XmlAttributes
  ): Attributes = {
    if (attr == Null)
      attributes
    else {
      XmlAttribute.create(ns, attr) match {
        case Left(newns) => _attrs(ns, attr.next, attributes)
        case Right(newattr) => _attrs(ns, attr.next, attributes + newattr)
      }
    }
  }

  private def _ns_attrs(ns: NamespaceBinding, elm: Elem): (NamespaceBinding, Attributes) =
    _ns_attrs(ns, elm.attributes, XmlAttributes.empty)

  private def _ns_attrs(
    ns: NamespaceBinding,
    attr: MetaData,
    attributes: XmlAttributes
  ): (NamespaceBinding, Attributes) = {
    if (attr == Null)
      (ns, attributes)
    else {
      XmlAttribute.create(ns, attr) match {
        case Left(newns) => _ns_attrs(newns, attr.next, attributes)
        case Right(newattr) => _ns_attrs(ns, attr.next, attributes + newattr)
      }
    }
  }

  private def _start_namespace_mapping(
    current: NamespaceBinding,
    base: NamespaceBinding
  ) {
    if (current != base && current != TopScope) {
      _start_namespace_mapping(current.parent, base)
      val prefix = Option(current.prefix) getOrElse ""
      _handler.startPrefixMapping(prefix, current.uri)
    }
  }

  private def _end_namespace_mapping(
    current: NamespaceBinding,
    base: NamespaceBinding
  ) {
    if (current != base && current != TopScope) {
      _end_namespace_mapping(current.parent, base)
      val prefix = Option(current.prefix) getOrElse ""
      _handler.endPrefixMapping(prefix)
    }
  }

  private def _traverse_special(node: SpecialNode) {
    node match {
      case m: Comment => Unit
      case m: EntityRef => Unit
      case m: PCData => _handler.characters(m.data.toCharArray, 0, m.data.length)
      case m: ProcInstr => _handler.processingInstruction(m.target, m.proctext)
      case m: Text => _handler.characters(m.data.toCharArray, 0, m.data.length)
      case m: Unparsed => Unit
    }
  }

  private def _traverse_children(ns: NamespaceBinding, node: Node) {
    _traverse_children(ns, node.child)
  }

  private def _traverse_children(ns: NamespaceBinding, nodes: Seq[Node]) {
    for (n <- nodes)
      _traverse_node(ns, n)
  }
}
