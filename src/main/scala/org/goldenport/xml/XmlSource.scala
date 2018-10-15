package org.goldenport.xml

import org.xml.sax.InputSource
import javax.xml.transform.sax.SAXSource

/*
 * @since   Feb. 22, 2016
 *  version Apr. 18, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
class XmlSource(node: scala.xml.Node) extends SAXSource {
  private val _reader = new XmlReader(node)

  override def getXMLReader() = _reader
  override def getSystemId(): String = "urn:XmlSource"
  override def getInputSource(): InputSource = new InputSource()
}
