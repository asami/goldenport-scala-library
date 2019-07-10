package org.goldenport.xml.dom

import scalaz.{Node => _, Writer => _, _}, Scalaz._
import scala.xml.{Node => XNode, Text => XText, Elem => XElem}
import scala.xml.{MetaData, TopScope, Null, PrefixedAttribute, UnprefixedAttribute, Group}
import java.io._
import org.cyberneko.html.parsers.DOMParser
import org.w3c.dom._
import org.xml.sax.InputSource
import javax.xml.transform._
import javax.xml.transform.stream._
import javax.xml.transform.dom._
import com.asamioffice.goldenport.xml.UXML
import org.goldenport.exception.RAISE
import org.goldenport.Strings
import org.goldenport.xml.{XmlSource, XmlUtils}

/*
 * See com.everforth.lib.xml.dom.DomUtils
 *
 * @since   Feb. 22, 2016
 *  version Mar. 31, 2016
 *  version Apr. 18, 2016
 *  version May. 13, 2016
 *  version Jun. 14, 2016
 *  version Oct. 12, 2017
 *  version May. 27, 2019
 * @version Jun. 30, 2019
 * @author  ASAMI, Tomoharu
 */
object DomUtils {
  val NS_HTML = "http://www.w3.org/1999/xhtml"
  val ATTR_ID = "id"
  val ATTR_CLASS = "class"

  def parseHtml(s: String): Document = {
    // http://nekohtml.sourceforge.net/faq.html
    // http://qiita.com/ak_nishiumi/items/56ce3c78868acfdcfb46
    val conf = new org.cyberneko.html.HTMLConfiguration()
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags", false)
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags/document-fragment", true) true causes a illegal behavior
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags/ignore-outside-content", true)
//   conf.setFeature("http://cyberneko.org/html/features/scanner/notify-builtin-refs", true)
    conf.setProperty("http://cyberneko.org/html/properties/names/elems", "match")
    conf.setProperty("http://cyberneko.org/html/properties/names/attrs", "no-change")
    conf.setProperty("http://cyberneko.org/html/properties/default-encoding", "UTF-8")
    val parser = new org.apache.xerces.parsers.DOMParser(conf)
    parser.parse(new InputSource(new StringReader(s)))
    parser.getDocument()
  }

  def parseHtmlLowerCase(s: String): Document = {
    // http://nekohtml.sourceforge.net/faq.html
    // http://qiita.com/ak_nishiumi/items/56ce3c78868acfdcfb46
    val conf = new org.cyberneko.html.HTMLConfiguration()
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags", false)
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags/document-fragment", true) true causes a illegal behavior
//   conf.setFeature("http://cyberneko.org/html/features/balance-tags/ignore-outside-content", true)
//   conf.setFeature("http://cyberneko.org/html/features/scanner/notify-builtin-refs", true)
    conf.setProperty("http://cyberneko.org/html/properties/names/elems", "lower")
    conf.setProperty("http://cyberneko.org/html/properties/names/attrs", "lower")
    conf.setProperty("http://cyberneko.org/html/properties/default-encoding", "UTF-8")
    val parser = new org.apache.xerces.parsers.DOMParser(conf)
    parser.parse(new InputSource(new StringReader(s)))
    parser.getDocument()
  }

  def parseXml(s: String): Document = {
    DomParser.parse(s)
  }

  def transformHtmlString(stylesheet: Templates, html: String): String = {
    val dom = transformHtml(stylesheet, html)
    toString(dom)
  }

  def transformHtmlString(stylesheet: String, html: String): String = {
    val dom = transformHtml(stylesheet, html)
    toString(dom)
  }

  def transformHtmlString(stylesheet: scala.xml.Node, html: String): String = {
    val dom = transformHtml(stylesheet, html)
    toString(dom)
  }

  def transformHtml(stylesheet: Templates, html: String): Node = {
    val dom = parseHtml(html)
    transform(stylesheet, dom)
  }

  def transformHtml(stylesheet: String, html: String): Node = {
    val dom = parseHtml(html)
    transform(stylesheet, dom)
  }

  def transformHtml(stylesheet: scala.xml.Node, html: String): Node = {
    val dom = parseHtml(html)
    transform(stylesheet, dom)
  }

  def parseHtmlFragment(s: String): Node = {
    val doc = parseHtml(s)
    if (_is_html_body(s))
      doc
    else
      _to_fragment(doc)
  }

  private def _is_html_body(s: String) = {
    s.indexOf("<HTML") != -1 || s.indexOf("<html") != -1 ||
    s.indexOf("<Html") != -1
  }

  private def _to_fragment(doc: Document) = {
    val html = doc.getDocumentElement
    if (isHtmlElement(html, "HTML")) {
      val elements = htmlElements(html)
      htmlElements(elements, "BODY").headOption match {
        case Some(s) => fragmentCloneElement(doc)(s)
        case None => htmlElements(elements, "HEAD").headOption match {
          case Some(ss) => fragmentCloneElement(doc)(ss)
          case None => fragmentCloneElement(doc)(html)
        }
      }
    } else if (isHtmlElement(html, "HEAD")) {
      fragmentCloneElement(doc)(html)
    } else {
      html
    }
  }

  // def parseHtmlFragment(s: String): Node = {
  //   val parser = new DOMParser()
  //   parser.parse(new InputSource(new StringReader(s)))
  //   val doc = parser.getDocument
  //   if (_is_html_body(s))
  //     doc
  //   else
  //     _to_fragment(doc)
  // }

  // private def _is_html_body(s: String) =
  //   s.indexOf("<HTML") != -1 || s.indexOf("<html") != -1

  // private def _to_fragment(doc: Document) = {
  //   val html = doc.getDocumentElement
  //   if (isHtmlElement(html, "HTML")) {
  //     val elements = htmlElements(html)
  //     htmlElements(elements, "BODY").headOption match {
  //       case Some(s) =>
  //         val xs = s.getChildNodes
  //         xs.getLength match {
  //           case 0 => s
  //           case 1 => xs.item(0)
  //           case _ => replaceCloneElement(doc)(s, "DIV")
  //         }
  //       case None => html
  //     }
  //   } else {
  //     html
  //   }
  // }

  def transformHtmlFragmentString(stylesheet: Templates, html: String): String = {
    val dom = transformHtmlFragment(stylesheet, html)
    toString(dom)
  }

  def transformHtmlFragmentString(stylesheet: String, html: String): String = {
    val dom = transformHtmlFragment(stylesheet, html)
    toString(dom)
  }

  def transformHtmlFragmentString(stylesheet: scala.xml.Node, html: String): String = {
    val dom = transformHtmlFragment(stylesheet, html)
    toString(dom)
  }

  def transformHtmlFragment(stylesheet: Templates, html: String): Node = {
    val dom = parseHtmlFragment(html)
    transform(stylesheet, dom)
  }

  def transformHtmlFragment(stylesheet: String, html: String): Node = {
    val dom = parseHtmlFragment(html)
    transform(stylesheet, dom)
  }

  def transformHtmlFragment(stylesheet: scala.xml.Node, html: String): Node = {
    val dom = parseHtmlFragment(html)
    transform(stylesheet, dom)
  }

  def transform(stylesheet: Templates, xml: String): Node = {
    val transformer = stylesheet.newTransformer()
    val source = new StreamSource(new StringReader(xml))
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  def transform(stylesheet: Templates, node: Node): Node = {
    val transformer = stylesheet.newTransformer()
    val source = new DOMSource(node)
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  // class DomBuilder() extends org.xml.sax.ContentHandler {
  //   def startDocument(): Unit = {}
  //   def endDocument(): Unit = {}
  //   def startElement(x$1: String,x$2: String,x$3: String,x$4: org.xml.sax.Attributes): Unit = {
  //     println(x$1)
  //     println(x$2)
  //     println(x$3)
  //     println(x$4)
  //   }
  //   def endElement(x$1: String,x$2: String,x$3: String): Unit = {}
  //   def startPrefixMapping(x$1: String,x$2: String): Unit = {}
  //   def endPrefixMapping(x$1: String): Unit = ???
  //   def characters(x$1: Array[Char],x$2: Int,x$3: Int): Unit = {
  //     println("characters")
  //     println(new String(x$1, x$2, x$3))
  //   }
  //   def ignorableWhitespace(x$1: Array[Char],x$2: Int,x$3: Int): Unit = {}
  //   def setDocumentLocator(x$1: org.xml.sax.Locator): Unit = {}
  //   def skippedEntity(x$1: String): Unit = {}
  //   def processingInstruction(x$1: String,x$2: String): Unit = {}
  // }

  def transform(stylesheet: String, xml: String): Node = {
    val stylesource = new StreamSource(new StringReader(stylesheet))
    val transformer = TransformerFactory.newInstance().newTransformer(stylesource)
    val source = new StreamSource(new StringReader(xml))
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  def transform(stylesheet: String, node: Node): Node = {
    val stylesource = new StreamSource(new StringReader(stylesheet))
    val transformer = TransformerFactory.newInstance().newTransformer(stylesource)
    val source = new DOMSource(node)
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  // Don't work when the stylesheet is a node.
  def transform(stylesheet: Node, node: Node): Node = {
    val stylesource = new DOMSource(stylesheet)
    val transformer = TransformerFactory.newInstance().newTransformer(stylesource)
    val source = new DOMSource(node)
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  def transform(stylesheet: scala.xml.Node, xml: String): Node = {
    val stylesource = new XmlSource(stylesheet)
    val transformer = TransformerFactory.newInstance().newTransformer(stylesource)
    val source = new StreamSource(new StringReader(xml))
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  def transform(stylesheet: scala.xml.Node, node: Node): Node = {
    val stylesource = new XmlSource(stylesheet)
    val transformer = TransformerFactory.newInstance().newTransformer(stylesource)
    val source = new DOMSource(node)
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  def newTemplates(stylesheet: String): Templates = {
    val stylesource = new StreamSource(new StringReader(stylesheet))
    TransformerFactory.newInstance().newTemplates(stylesource)
  }

  def newTemplates(stylesheet: Node): Templates = {
    val stylesource = new DOMSource(stylesheet)
    TransformerFactory.newInstance().newTemplates(stylesource)
  }

  def newTemplates(stylesheet: scala.xml.Node): Templates = {
    val stylesource = new XmlSource(stylesheet)
    TransformerFactory.newInstance().newTemplates(stylesource)
  }

  def toText(node: Node): String = toString(node)

  def toString(node: Node): String = {
    val buf = new StringWriter()
    printDocument(node, buf)
    buf.toString
  }

  def printDocument(node: Node, out: Writer) {
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.setOutputProperty(OutputKeys.METHOD, "xml")
    transformer.setOutputProperty(OutputKeys.INDENT, "no")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.transform(new DOMSource(node), new StreamResult(out))
  }

  def printDocumentPretty(node: Node, out: Writer) {
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
    transformer.setOutputProperty(OutputKeys.METHOD, "xml")
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
    transformer.transform(new DOMSource(node), new StreamResult(out))
  }

  // def toHtmlFragmentText(node: Node): String =
  //   com.everforth.lib.xml.dom.DomUtils.toHtmlFragmentText(node)

  // TODO migrate to xmllib
  def toHtmlFragmentText(node: Node): String = {
    import com.asamioffice.goldenport.xml.XMLMaker
    val maker = new XMLMaker()
    maker.setEmptyElementTag(false)
    maker.setEncoding("UTF-8")
    maker.setAutoMixed(false)
    maker.make(node)
    maker.getText()
  }

  // def toHtmlFragmentText(p: Node): String = {
  //   val buf = new StringBuilder
  //   _to_html_fragment_text(buf, p)
  //   buf.toString
  // }

  // private def _to_html_fragment_text(buffer: StringBuilder, p: Node) {
  //   def toattr(a: Attr) {
  //     val ans = a.getNamespaceURI
  //     val aname = a.getName
  //     val avalue = a.getValue
  //     val aisid = a.isId
  //     val aspecified = a.getSpecified
  //     val typeinfo = a.getSchemaTypeInfo
  //     buffer.append(aname)
  //     buffer.append("=\"")
  //     buffer.append(escapeAttrQuot(avalue))
  //     buffer.append("\"")
  //   }
  //   p match {
  //     case m: Element =>
  //       val tagname = m.getTagName
  //       val attrs = DomUtils.attributes(m)
  //       buffer.append("<")
  //       buffer.append(tagname)
  //       attrs.headOption map { x =>
  //         toattr(x)
  //         for (xx <- attrs.tail) {
  //           buffer.append(" ")
  //           toattr(x)
  //         }
  //       }
  //       buffer.append(">")
  //       DomUtils.children(m).foreach(_to_html_fragment_text(buffer, _))
  //       buffer.append("</")
  //       buffer.append(tagname)
  //       buffer.append(">")
  //     case m: CDATASection => buffer.append(escapeCharDataCr(m.getTextContent))
  //     case m: Text => buffer.append(escapeCharDataCr(m.getTextContent))
  //     case m: Comment => 
  //     case m: DocumentFragment => DomUtils.children(m).foreach(_to_html_fragment_text(buffer, _))
  //     case m: EntityReference => ???
  //     case m: ProcessingInstruction => ???
  //   }
  // }

  // TODO migrate to xmllib
  def escape(s: String) = UXML.escape(s)
  def escapeEntityQuot(s: String) = UXML.escapeEntityQuot(s)
  def escapeEntityApos(s: String) = UXML.escapeEntityApos(s)
  def escapeAttrQuot(s: String) = UXML.escapeAttrQuot(s)
  def escapeAttrApos(s: String) = UXML.escapeAttrApos(s)
  def escapeSystemQuot(s: String) = UXML.escapeSystemQuot(s)
  def escapeSystemApos(s: String) = UXML.escapeSystemApos(s)
  def escapeCharData(s: String) = UXML.escapeCharData(s)
  def escapeCharDataCr(s: String) = UXML.escapeCharDataCr(s)

  def toHtmlText(node: Node): String = {
    val buf = new StringWriter()
    printHtml(node, buf)
    buf.toString
  }

  def printHtml(node: Node, out: Writer) {
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.setOutputProperty(OutputKeys.METHOD, "html")
    transformer.setOutputProperty(OutputKeys.INDENT, "no")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.transform(new DOMSource(node), new StreamResult(out))
  }

  def printHtmlPretty(node: Node, out: Writer) {
    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
    transformer.setOutputProperty(OutputKeys.METHOD, "html")
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
    transformer.transform(new DOMSource(node), new StreamResult(out))
  }

  def tree(node: Node): Tree[Node] =
    Tree.node(node, DomUtils.childrenSteream(node).map(tree))

  def showDom(node: Node): String = {
    implicit object TreeShows extends Show[Node] {
      override def show(p: Node): Cord = p match {
        case m: Element => m.toString
        case m: Text => m.toString
        case m => m.toString
      }
    }
    tree(node).drawTree
  }

  def localName(element: Element): String =
    Option(element.getLocalName) getOrElse element.getTagName

  def htmlElements(node: Node, localname: String): List[Element] =
    htmlElements(children(node), localname)

  def htmlElements(xs: List[Node], localname: String): List[Element] =
    xs collect {
      case elem: Element if isHtmlElement(elem, localname) => elem
    }

  def isHtmlElement(elem: Element, localname: String): Boolean =
    isHtmlElement(elem) && localName(elem).equalsIgnoreCase(localname)

  def isHtmlElement(elem: Element): Boolean = {
    val ns = elem.getNamespaceURI
    (ns == null || ns == NS_HTML)
  }

  def htmlElements(node: Node): List[Element] = {
    children(node) collect {
      case elem: Element if isHtmlElement(elem) => elem
    }
  }

  def childrenIndexedSeq(node: Node): IndexedSeq[Node] = {
    val xs = node.getChildNodes
    for (i <- 0 until xs.getLength) yield xs.item(i)
  }

  def children(node: Node): List[Node] =
    childrenIndexedSeq(node).toList

  def childrenSteream(node: Node): Stream[Node] =
    childrenIndexedSeq(node).toStream

  def elementsIndexedSeq(node: Node): IndexedSeq[Element] =
    childrenIndexedSeq(node) collect {
      case m: Element => m
    }

  def elements(node: Node): List[Element] =
    elementsIndexedSeq(node).toList

  def elementsSteream(node: Node): Stream[Element] =
    elementsIndexedSeq(node).toStream

  def elementsByLocalNameIC(node: Node, localname: String): List[Element] =
    elementsVectorByLocalNameIC(node, localname).toList

  def elementsByLocalNameIC(node: Node, localname: String, localname2: String, localnames: String*): List[Element] =
    elementsVectorByLocalNameIC(node, localname, localname2, localnames: _*).toList

  def elementsVectorByLocalNameIC(node: Node, localname: String): Vector[Element] =
    childrenIndexedSeq(node).collect {
      case m: Element => m
    }.filter(x => localname.equalsIgnoreCase(x.getLocalName)).toVector

  def elementsVectorByLocalNameIC(node: Node, localname: String, localname2: String, localnames: String*): Vector[Element] = {
    val names = (localnames.toVector :+ localname :+ localname2).map(_.toLowerCase)
    childrenIndexedSeq(node).collect {
      case m: Element => m
    }.filter(x =>
      names.contains(Option(x.getLocalName).map(_.toLowerCase).getOrElse(""))
    ).toVector
  }

  def copyNode(doc: Document)(src: Node): Node = src match {
    case m: CDATASection => copyCDATASection(doc)(m)
    case m: Comment => copyComment(doc)(m)
    case m: DocumentFragment => copyDocumentFragment(doc)(m)
    case m: Element => copyElement(doc)(m)
    case m: EntityReference => copyEntityReference(doc)(m)
    case m: ProcessingInstruction => copyProcessingInstruction(doc)(m)
    case m: Text => copyText(doc)(m)
  }

  def copyCDATASection(doc: Document)(src: CDATASection): CDATASection =
    doc.createCDATASection(src.getTextContent)

  def copyComment(doc: Document)(src: Comment): Comment =
    doc.createComment(src.getTextContent)

  def copyDocumentFragment(doc: Document)(src: DocumentFragment): DocumentFragment =
    doc.createDocumentFragment()

  def copyElement(doc: Document)(src: Element): Element = {
    val tagname = src.getTagName
    val ns = src.getNamespaceURI
    val elem = if (ns == null)
      doc.createElement(tagname)
    else
      doc.createElementNS(ns, tagname)
    for (a <- attributes(src)) {
      val ans = a.getNamespaceURI
      val aname = a.getName
      val avalue = a.getValue
      val aisid = a.isId
      val aspecified = a.getSpecified
      val typeinfo = a.getSchemaTypeInfo
      if (ans == null) {
        elem.setAttribute(aname, avalue)
//        elem.setAttribute(aname, aisid) DOM Level 3
      } else {
        elem.setAttributeNS(ans, aname, avalue)
//        elem.setAttributeNS(ans, aname, aisid) DOM Level 3
      }
    }
    // TODO children ?
    elem
  }

  def copyEntityReference(doc: Document)(src: EntityReference): EntityReference =
    doc.createEntityReference(src.getTextContent)

  def copyProcessingInstruction(doc: Document)(src: ProcessingInstruction): ProcessingInstruction =
    doc.createProcessingInstruction(src.getTarget, src.getData)

  def copyText(doc: Document)(src: Text): Text =
    doc.createTextNode(src.getTextContent)

  def attributes(element: Element): List[Attr] = {
    val attrs = element.getAttributes
    if (attrs == null)
      Nil
    else
      (0 until attrs.getLength).toList flatMap { i =>
        attrs.item(i) match {
          case m: Attr => Some(m)
          case _ => None
        }
      }
  }

  def getAttributeIgnoreCase(element: Element, name: String): Option[String] = {
    val fp = isMatchLocalNameIgnoreCase(Set(name))
    attributes(element).find(fp).map(_.getValue)
  }

  // XXX lib.xml.dom
  def findElement(p: Element => Boolean)(node: Node): Option[Element] =
    findElementBreadth(p)(node)

  // XXX lib.xml.dom
  def findElementBreadth(p: Element => Boolean)(node: Node): Option[Element] = {
    node match {
      case m: Element if p(m) => Some(m)
      case _ =>
        val xs = elements(node)
        xs.find(isMatch(p)) orElse xs.toStream.flatMap(findElementBreadth(p)).headOption
    }
  }

  // XXX lib.xml.dom
  def findElementDepth(p: Element => Boolean)(node: Node): Option[Element] = {
    node match {
      case m: Element if p(m) => Some(m)
      case _ =>
        val xs = elements(node)
        xs.toStream.flatMap(findElementBreadth(p)).headOption
    }
  }

  def getElementByLocalNameIC(node: Node, localname: String): Option[Element] =
    elementsVectorByLocalNameIC(node, localname).headOption

  def getElementByLocalNameIC(node: Node, localname: String, localname2: String, localnames: String*): Option[Element] =
    elementsVectorByLocalNameIC(node, localname, localname2, localnames: _*).headOption

  def isMatch(p: Element => Boolean)(node: Node): Boolean =
    node match {
      case m: Element => p(m)
      case _ => false
    }

  def isMatchClass(name: String)(node: Node): Boolean =
    node match {
      case m: Element if m.getAttribute("class") == name => true
      case _ => false
    }

  def isMatchLocalName(names: Set[String])(node: Node): Boolean =
    node match {
      case m: Element if names.contains(localName(m)) => true
      case _ => false
    }

  def isMatchLocalNameIgnoreCase(names: Set[String]): Node => Boolean = {
    val uppernames = names.map(_.toUpperCase)
    isMatch(elem => uppernames.contains(localName(elem).toUpperCase))
  }

  def attributesToMap(p: NamedNodeMap): Map[String, String] = {
    val length = p.getLength
    (0 until length).map { i =>
      val node = p.item(i)
      node.getNodeName -> node.getNodeValue
    }.toMap
  }

  def cloneNode(doc: Document)(src: Node): Node = {
    val p = copyNode(doc)(src)
    src match {
      case m: Element => cloneChildren(doc)(p.asInstanceOf[Element], childrenIndexedSeq(src))
      case _ => ()
    }
    p
  }
  def replaceCloneElement(doc: Document)(src: Element, tagname: String): Element = {
    val p = doc.createElement(tagname)
    cloneChildren(doc)(p, childrenIndexedSeq(src))
  }

  def fragmentCloneElement(doc: Document)(src: Element): Node = {
    val xs = src.getChildNodes
    xs.getLength match {
      case 0 => doc.createDocumentFragment()
      case 1 => xs.item(0)
      case n =>
        val a = doc.createDocumentFragment()
        for (i <- 0 until n)
          a.appendChild(cloneNode(doc)(xs.item(i)))
        a
    }
  }

  def cloneChildren(doc: Document)(target: Element, children: Seq[Node]): Element = {
    for (c <- children)
      target.appendChild(cloneNode(doc)(c))
    target
  }

  def copyChildren(doc: Document)(target: Element, children: Seq[Node]): Element = {
    for (c <- children)
      target.appendChild(copyNode(doc)(c))
    target
  }

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

  private def _distill_text_dom(elem: Element): String = {
    val builder = new StringBuilder
    val a = elem.getElementsByTagName("body")
    if (a.getLength() == 0) {
      _distill_text(a.item(0), builder)
    } else {
      _distill_text(elem, builder)
    }
    builder.toString
  }

  private def _distill_text(n: Node, builder: StringBuilder) {
    n match {
      case t: Text => builder.append(t.getWholeText)
      case _ => {
        val xs = n.getChildNodes()
        for (i <- 0 until xs.getLength) {
          _distill_text(xs.item(i), builder)
        }
      }
    }
  }

  def toXml(p: Node): XNode = p match {
    case m: Element => _to_element(m)
    case m: Text => XText(m.getWholeText)
    case m: Document => toXml(m.getDocumentElement)
    case m: DocumentFragment =>
      val a = DomUtils.children(m).flatMap(toXmlOption)
      XmlUtils.nodesToNodeOption(a).getOrElse(Group(Nil))
    case m => RAISE.notImplementedYetDefect
  }

  def toXmlOption(p: Node): Option[XNode] = p match {
    case m: DocumentFragment =>
      val a = DomUtils.children(m).flatMap(toXmlOption)
      XmlUtils.nodesToNodeOption(a)
    case m => Some(toXml(p))
  }


  private def _to_element(p: Element): XElem = {
    val tagname = p.getTagName
    val prefix = p.getPrefix
    val label = localName(p)
    val ns = p.getNamespaceURI
    val attrs = _to_attributes(attributes(p))
    val scope = TopScope // TODO
    val cs: Seq[XNode] = children(p).map(toXml)
    val minimizeempty = false
    new XElem(prefix, label, attrs, scope, minimizeempty, cs: _*)
  }

  private def _to_attributes(ps: List[Attr]): MetaData = ps match {
    case Nil => Null
    case x :: xs => _to_attribute(x, _to_attributes(xs))
  }

  private def _to_attribute(p: Attr, next: MetaData): MetaData =
    Option(p.getPrefix).cata(
      new PrefixedAttribute(_, p.getName, p.getValue, next),
      new UnprefixedAttribute(p.getName, p.getValue, next)
    )

//       val ans = a.getNamespaceURI
//       val aname = a.getName
//       val avalue = a.getValue
//       val aisid = a.isId
//       val aspecified = a.getSpecified
//       val typeinfo = a.getSchemaTypeInfo
//       if (ans == null) {
//         elem.setAttribute(aname, avalue)
// //        elem.setAttribute(aname, aisid) DOM Level 3
//       } else {
//         elem.setAttributeNS(ans, aname, avalue)
// //        elem.setAttributeNS(ans, aname, aisid) DOM Level 3
//       }
//     }

}
