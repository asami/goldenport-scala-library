package org.goldenport.xml.dom

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._
import org.goldenport.RAISE

/*
 * @since   Jun. 29, 2019
 * @version Jul. 10, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class DomUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import DomUtils._

  val xml = """<PHONEBOOK>
    <PERSON>
        <NAME>Ren1</NAME>
        <EMAIL>ren1@gmail.com</EMAIL>
        <TELEPHONE>999-999-9999</TELEPHONE>
        <WEB>www.ren1.com</WEB>
    </PERSON>
    <PERSON>
        <NAME>Ren2</NAME>
        <EMAIL>ren2@gmail.com</EMAIL>
        <TELEPHONE>999-999-9999</TELEPHONE>
        <WEB>www.ren2.com</WEB>
    </PERSON>
    <PERSON>
        <NAME>Ren3</NAME>
        <EMAIL>ren3@gmail.com</EMAIL>
        <TELEPHONE>999-999-9999</TELEPHONE>
        <WEB>www.ren3.com</WEB>
    </PERSON>
</PHONEBOOK>"""

  val ss = """<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">
    <xsl:output omit-xml-declaration="yes" method="xml"></xsl:output>

    <xsl:template match="/">

        <Names>
            <xsl:for-each select="PHONEBOOK/PERSON">
                <Name>

                    <xsl:value-of select="NAME" />
                </Name>
            </xsl:for-each>
        </Names>
  </xsl:template>
</xsl:stylesheet>
"""

  val xmldom = parseXml(xml)
  val ssdom = parseXml(ss)

  "DomUtils" should {
    "transform" which {
      "string stylesheet, string target" in {
        val doc = transform(ss, xml)
        val r = doc match {
          case m: org.w3c.dom.Document => m.getDocumentElement
          case m: org.w3c.dom.Element => RAISE.noReachDefect
          case m: org.w3c.dom.Node => RAISE.noReachDefect
        }
        println(r)
      }
      "string stylesheet, dom target" in {
        val doc = transform(ss, xmldom)
        val r = doc match {
          case m: org.w3c.dom.Document => m.getDocumentElement
          case m: org.w3c.dom.Element => RAISE.noReachDefect
          case m: org.w3c.dom.Node => RAISE.noReachDefect
        }
        println(r)
      }
    }
  }
}
