package org.goldenport.xml.dom

import org.w3c.dom._
import org.goldenport.util.StringUtils

/*
 * @since   Mar. 31, 2016
 *  version Apr. 21, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
trait DomAction {
  def apply(doc: Document)(node: Node): (DomAction, DomActionStrategy)
}

object DomAction {
  def transform(action: DomAction)(src: Node): Node =
    transform(src.getOwnerDocument, action)(src)

  def transform(doc: Document, action: DomAction)(src: Node): Node = {
    val (newaction, strategy) = action(doc)(src)
    strategy match {
      case EndStrategy => doc.createDocumentFragment
      case NodeCopyChildrenTransformStrategy => 
        _transform(doc, newaction)(src)
      case NodeCreatedChildrenTransformStrategy(dest) => 
        _transform_children(doc, newaction)(src, dest)
      case NodeCreatedChildrenCreatedStrategy(dest) => dest
      case NodeRemoveChildrenTransformStrategy =>
        _transform_descent(doc, newaction)(src)
    }
  }

  private def _transform(doc: Document, action: DomAction)(src: Node): Node =
    _transform_children(doc, action)(src, DomUtils.copyNode(doc)(src))

  private def _transform_children(doc: Document, action: DomAction)(src: Node, dest: Node): Node = {
    for (c <- DomUtils.children(src))
      dest.appendChild(transform(doc, action)(c))
    dest
  }

  private def _transform_descent(doc: Document, action: DomAction)(src: Node): Node = {
    val xs = DomUtils.children(src).map(transform(doc, action)).
      filter(_.getNodeType != Node.DOCUMENT_FRAGMENT_NODE)
    xs match {
      case Nil => doc.createDocumentFragment
      case x :: Nil => x
      case xs =>
        val r = doc.createDocumentFragment
        for (x <- xs)
          r.appendChild(x)
        r
    }
  }
}

case object NopClassAction extends DomAction {
  def apply(doc: Document)(node: Node) =
    (this, NodeCopyChildrenTransformStrategy)
}

case class RemoveClassAction(localname: String) extends DomAction {
  def apply(doc: Document)(node: Node) =
    node match {
      case m: Element if m.getAttribute("class") == localname =>
        (NopClassAction, EndStrategy)
      case _ => (this, NodeCopyChildrenTransformStrategy)
    }
}

case class GlanceClassAction(classname: String, length: Int = 200) extends DomAction {
  def apply(doc: Document)(node: Node) =
    node match {
      case m: Element if m.getAttribute("class") == classname =>
        val a1 = Option(m.getTextContent) getOrElse ""
        val a2 = StringUtils.dimString(a1, length)
        val a = doc.createTextNode(a2)
        val dest = DomUtils.copyElement(doc)(m)
        dest.appendChild(a)
        (NopClassAction, NodeCreatedChildrenCreatedStrategy(dest))
      case _ => (this, NodeCopyChildrenTransformStrategy)
    }
}

case class FindClassAction(classname: String) extends DomAction {
  def apply(doc: Document)(node: Node) =
    node match {
      case m: Element if m.getAttribute("class") == classname =>
        (NopClassAction, NodeCopyChildrenTransformStrategy)
      case _ => (this, NodeRemoveChildrenTransformStrategy)
    }
}

sealed trait DomActionStrategy
case object EndStrategy extends DomActionStrategy
case object NodeCopyChildrenTransformStrategy extends DomActionStrategy
case class NodeCreatedChildrenTransformStrategy(node: Node) extends DomActionStrategy
case class NodeCreatedChildrenCreatedStrategy(node: Node) extends DomActionStrategy
case object NodeRemoveChildrenTransformStrategy extends DomActionStrategy
