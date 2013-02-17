package org.goldenport.xml

import scala.xml._
import scala.xml.parsing._
import java.io.{FileReader, StringReader}
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.xml.sax.InputSource

/**
 * @since   Jul. 27, 2010
 *  version Oct.  8, 2010
 * @version Feb. 17, 2013
 * @author  ASAMI, Tomoharu
 */
object GXml {
  // dom
  // sax
  // namespace
  // xpath
  // xslt
  def transform(xslt: Elem)(source: Elem): Elem = {
    val saxHandler = new NoBindingFactoryAdapter()
    saxHandler.scopeStack.push(TopScope)
    val transFactory = TransformerFactory.newInstance
    val xslts = new StreamSource(new StringReader(xslt.toString))
    val trans = transFactory.newTransformer(xslts)
    val ss = new StreamSource(new StringReader(source.toString))
    trans.transform(ss, new SAXResult(saxHandler))
    saxHandler.scopeStack.pop
    saxHandler.rootElem.asInstanceOf[Elem]
  }

  def normalizeNsPrefix(map: Map[String, String])(source: Elem): Elem = {
    sys.error("XXX")
  }

  def attribute(node: Node, key: String): Option[String] = {
    node.attribute(key) match {
      case Some(value) => Some(value.map(_.text).mkString)
      case None => None
    }
  }

  def attributeText(attr: Attribute): String = {
    attr.value.map(_.text).mkString
  }
}
