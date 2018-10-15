package org.goldenport.xml.dom

import org.w3c.dom._

/*
 * @since   Apr. 11, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
trait DomTransformerHelper {
  import DomUtils._

  def document: Document

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
}
