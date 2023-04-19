package org.goldenport.xml.dom.text

import org.w3c.dom._

/*
 * @since   Apr. 18, 2023
 * @version Apr. 19, 2023
 * @author  ASAMI, Tomoharu
 */
abstract class TextMaker(
) extends com.asamioffice.goldenport.text.TextMaker {
  protected def make_text(node: Node): Unit = node match {
    case m: Document => make_Document(m)
    case m: CDATASection => make_CDATASection(m)
    case m: Comment => make_Comment(m)
    case m: DocumentFragment => make_DocumentFragment(m)
    case m: Element => make_Element(m)
    case m: EntityReference => make_EntityReference(m)
    case m: ProcessingInstruction => make_ProcessingInstruction(m)
    case m: Text => make_Text(m)
  }

  protected def make_Document(p: Document): Unit = make_text(p.getDocumentElement)

  protected def make_CDATASection(p: CDATASection): Unit = {}

  protected def make_Comment(p: Comment): Unit = {}

  protected def make_DocumentFragment(p: DocumentFragment): Unit = {}

  protected def make_Element(p: Element): Unit

  protected def make_EntityReference(p: EntityReference): Unit = {}

  protected def make_ProcessingInstruction(p: ProcessingInstruction): Unit = {}

  protected def make_Text(p: Text): Unit = {
    print(p.getWholeText)
  }
}

object TextMaker {
}
