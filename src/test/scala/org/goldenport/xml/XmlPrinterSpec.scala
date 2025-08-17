package org.goldenport.xml

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import scala.xml._

/*
 * @since   Mar. 29, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class XmlPrinterSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "XmlPrinter" should {
    "pretty" which {
      "typical" in {
        val printer = XmlPrinter.htmlFragment
        val s = printer(<p>abc</p>)
        s should be("<p>abc</p>\n")
      }
      "inline" in {
        val printer = XmlPrinter.htmlFragment
        val s = printer(<p>a<b>b</b>c</p>)
        s should be("<p>a<b>b</b>c</p>\n")
      }
      "div" in {
        val printer = XmlPrinter.htmlFragment
        val s = printer(<div><p>a<b>b</b>c</p></div>)
        s should be("<div>\n  <p>a<b>b</b>c</p>\n</div>\n")
      }
      "div2" in {
        val printer = XmlPrinter.htmlFragment
        val s = printer(<div><div><p>a<b>b</b>c</p></div></div>)
        s should be("<div>\n  <div>\n    <p>a<b>b</b>c</p>\n  </div>\n</div>\n")
      }
      "space" in {
        val printer = XmlPrinter.htmlFragment
        val s = printer(<div>     <div><p>a<b>b</b>c</p></div>  </div>)
        s should be("<div>\n  <div>\n    <p>a<b>b</b>c</p>\n  </div>\n</div>\n")
      }
    }
  }
}
