package com.asamioffice.goldenport.xml

/*
 * @since   Dec.  4, 2012
 * @version Dec.  4, 2012
 * @author  ASAMI, Tomoharu
 */
trait XmlUtil {
  def prettyString(s: String): String = {
    val d = Parser.parseString(s)
    UXMLMaker.getXMLVisualText(d)
  }
}

object XmlUtil extends XmlUtil {
}
