package org.goldenport.xml.dom

import org.xml.sax.{InputSource, SAXException}
import org.w3c.dom._
import java.io._
import javax.xml.parsers._

/*
 * @since   Apr. 14, 2016
 *  version Oct. 12, 2017
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
object DomParser {
  lazy val factory = DocumentBuilderFactory.newInstance()

  def parse(s: String): Document = {
    val builder = factory.newDocumentBuilder()
    // val in = new ByteArrayInputStream(s.getBytes("utf-8"))
    // builder.parse(in)
    builder.parse(new InputSource(new StringReader(s)))
  }
}
