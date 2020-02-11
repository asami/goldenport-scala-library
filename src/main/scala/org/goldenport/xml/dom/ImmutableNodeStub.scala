package org.goldenport.xml.dom

import org.w3c.dom._
import org.goldenport.RAISE

/*
 * @since   Nov. 29, 2019
 * @version Nov. 29, 2019
 * @author  ASAMI, Tomoharu
 */
trait ImmutableNodeStub { self: Node =>
    def compareDocumentPosition(node: Node): Short = RAISE.unsupportedOperationFault(this, "compareDocumentPosition")
    def getAttributes(): NamedNodeMap = RAISE.unsupportedOperationFault(this, "getAttributes")
    def getBaseURI(): String = RAISE.unsupportedOperationFault(this, "getBaseURI")
    def getChildNodes(): NodeList = RAISE.unsupportedOperationFault(this, "getChildNodes")
    def getFeature(x$1: String,x$2: String): Object = RAISE.unsupportedOperationFault(this, "getFeature")
    def getFirstChild(): Node = RAISE.unsupportedOperationFault(this, "getFirstChild")
    def getLastChild(): Node = RAISE.unsupportedOperationFault(this, "getLastChild")
    def getLocalName(): String = RAISE.unsupportedOperationFault(this, "getLocalName")
    def getNamespaceURI(): String = RAISE.unsupportedOperationFault(this, "getNamespaceURI")
    def getNextSibling(): Node = RAISE.unsupportedOperationFault(this, "getNextSibling")
    def getNodeName(): String = RAISE.unsupportedOperationFault(this, "getNodeName")
    def getNodeValue(): String = RAISE.unsupportedOperationFault(this, "getNodeValue")
    def getOwnerDocument(): Document = RAISE.unsupportedOperationFault(this, "getOwnerDocument")
    def getParentNode(): Node = RAISE.unsupportedOperationFault(this, "getParentNode")
    def getPrefix(): String = RAISE.unsupportedOperationFault(this, "getPrefix")
    def getPreviousSibling(): Node = RAISE.unsupportedOperationFault(this, "getPreviousSibling")
    def getTextContent(): String = RAISE.unsupportedOperationFault(this, "getTextContent")
    def getUserData(x$1: String): Object = RAISE.unsupportedOperationFault(this, "getUserData")
    def hasAttributes(): Boolean = RAISE.unsupportedOperationFault(this, "hasAttributes")
    def hasChildNodes(): Boolean = RAISE.unsupportedOperationFault(this, "hasChildNodes")
    def isDefaultNamespace(x$1: String): Boolean = RAISE.unsupportedOperationFault(this, "isDefaultNamespace")
    def isEqualNode(node: Node): Boolean = RAISE.unsupportedOperationFault(this, "isEqualNode")
    def isSameNode(node: Node): Boolean = RAISE.unsupportedOperationFault(this, "isSameNode")
    def isSupported(x$1: String,x$2: String): Boolean = RAISE.unsupportedOperationFault(this, "isSupported")
    def lookupNamespaceURI(x$1: String): String = RAISE.unsupportedOperationFault(this, "lookupNamespaceURI")
    def lookupPrefix(x$1: String): String = RAISE.unsupportedOperationFault(this, "lookupPrefix")
}
