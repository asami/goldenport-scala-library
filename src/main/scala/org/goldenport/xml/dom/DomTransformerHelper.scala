package org.goldenport.xml.dom

import javax.xml.xpath._
import javax.xml.transform._
// import javax.xml.transform.stream._
import javax.xml.transform.dom._
import org.w3c.dom._
import org.goldenport.Strings

/*
 * @since   Apr. 11, 2016
 *  version Oct. 12, 2017
 * @version Apr. 19, 2023
 * @author  ASAMI, Tomoharu
 */
trait DomTransformerHelper {
  import DomUtils._

  def document: Document

  private lazy val _xpath_factory = XPathFactory.newInstance()

  protected def transform_node(node: Node): Node = node match {
    case m: CDATASection => transform_CDATASection(m)
    case m: Comment => transform_Comment(m)
    case m: DocumentFragment => transform_DocumentFragment(m)
    case m: Element => transform_Element(m)
    case m: EntityReference => transform_EntityReference(m)
    case m: ProcessingInstruction => transform_ProcessingInstruction(m)
    case m: Text => transform_Text(m)
  }

  protected def transform_CDATASection(p: CDATASection) =
    copyCDATASection(document)(p)

  protected def transform_Comment(p: Comment) =
    copyComment(document)(p)

  protected def transform_DocumentFragment(p: DocumentFragment) =
    copyDocumentFragment(document)(p)

  protected def transform_Element(p: Element) = copy_element(p)

  protected def transform_EntityReference(p: EntityReference) =
    copyEntityReference(document)(p)

  protected def transform_ProcessingInstruction(p: ProcessingInstruction) =
    copyProcessingInstruction(document)(p)

  protected def transform_Text(p: Text) =
    copyText(document)(p)

  protected def copy_node(node: Node): Node = copyNode(document)(node)

  protected def copy_element(elem: Element): Element = copyElement(document)(elem)

  protected final def transform_xpath(path: String, p: Node): Node = {
    val xpath = _xpath_factory.newXPath()
    val expr = xpath.compile(path)
    transform_xpath(expr, p)
  }

  protected final def transform_xpath(expr: XPathExpression, p: Node): Node = {
    val r = expr.evaluate(p, XPathConstants.NODESET).asInstanceOf[NodeList]
    DomUtils.toFragment(document)(r)
  }

  protected final def transform_xslt(transformer: Transformer, p: Node): Node = {
    val source = new DOMSource(p)
    val result = new DOMResult()
    transformer.transform(source, result)
    result.getNode
  }

  protected final def is_empty_text(p: Node): Boolean = !is_non_empty_text(p)

  protected final def is_non_empty_text(p: Node): Boolean = {
    def _go_(node: Node): Boolean = Option(node).collect {
      case m: Text => Strings.notblankp(m.getWholeText)
      case m: Document => _go_(m.getDocumentElement)
      case m: DocumentFragment =>
        DomUtils.childrenSteream(m).
          flatMap(x => if (_go_(x)) Some(true) else None).headOption.getOrElse(false)
      case m: Element =>
        DomUtils.childrenSteream(m).
          flatMap(x => if (_go_(x)) Some(true) else None).headOption.getOrElse(false)
      case _ => false
    }.getOrElse(false)
    _go_(p)
  }
}
