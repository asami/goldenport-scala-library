package org.goldenport.xml.dom

import org.w3c.dom._
import org.goldenport.RAISE

/*
 * @since   Aug. 23, 2019
 * @version Aug. 23, 2019
 * @author  ASAMI, Tomoharu
 */
trait ImmutableNodeImpl { self: Node =>
  def appendChild(node: Node): Node = RAISE.unsupportedOperationFault(this, "appendChild")
  def insertBefore(p: Node, q: Node): Node = RAISE.unsupportedOperationFault(this, s"insertBefore:${p}, ${q}")
  def cloneNode(deep: Boolean): Node = this
  def normalize(): Unit = Unit
  // def compareDocumentPosition(node: Node): Short = RAISE.unsupportedOperationFault
  // def getAttributes(): NamedNodeMap = RAISE.unsupportedOperationFault
  // def getBaseURI(): String = RAISE.unsupportedOperationFault
  // def getChildNodes(): NodeList = RAISE.unsupportedOperationFault
  // def getFeature(x$1: String,x$2: String): Object = RAISE.unsupportedOperationFault
  // def getFirstChild(): Node = RAISE.unsupportedOperationFault
  // def getLastChild(): Node = RAISE.unsupportedOperationFault
  // def getLocalName(): String = RAISE.unsupportedOperationFault
  // def getNamespaceURI(): String = RAISE.unsupportedOperationFault
  // def getNextSibling(): Node = RAISE.unsupportedOperationFault
  // def getNodeName(): String = RAISE.unsupportedOperationFault
  // def getNodeType(): Short = RAISE.unsupportedOperationFault
  // def getNodeValue(): String = RAISE.unsupportedOperationFault
  // def getOwnerDocument(): Document = RAISE.unsupportedOperationFault
  // def getParentNode(): Node = RAISE.unsupportedOperationFault
  // def getPrefix(): String = RAISE.unsupportedOperationFault
  // def getPreviousSibling(): Node = RAISE.unsupportedOperationFault
  // def getTextContent(): String = RAISE.unsupportedOperationFault
  // def getUserData(x$1: String): Object = RAISE.unsupportedOperationFault
  // def hasAttributes(): Boolean = RAISE.unsupportedOperationFault
  // def hasChildNodes(): Boolean = RAISE.unsupportedOperationFault
  // def isDefaultNamespace(x$1: String): Boolean = RAISE.unsupportedOperationFault
  // def isEqualNode(node: Node): Boolean = RAISE.unsupportedOperationFault
  // def isSameNode(node: Node): Boolean = RAISE.unsupportedOperationFault
  // def isSupported(x$1: String,x$2: String): Boolean = RAISE.unsupportedOperationFault
  // def lookupNamespaceURI(x$1: String): String = RAISE.unsupportedOperationFault
  // def lookupPrefix(x$1: String): String = RAISE.unsupportedOperationFault
  def removeChild(node: Node): Node = RAISE.unsupportedOperationFault(this, "removeChild")
  def replaceChild(node: Node, node2: Node): Node = RAISE.unsupportedOperationFault(this, "replaceChild")
  def setNodeValue(value: String): Unit = RAISE.unsupportedOperationFault(this, "setNodeValue")
  def setPrefix(prefix: String): Unit = RAISE.unsupportedOperationFault(this, "setPrefix")
  def setTextContent(text: String): Unit = RAISE.unsupportedOperationFault(this, "setTextContent")
  def setUserData(key: String, data: Any, handler: UserDataHandler): Object = RAISE.unsupportedOperationFault(this, "setUserData")
}
