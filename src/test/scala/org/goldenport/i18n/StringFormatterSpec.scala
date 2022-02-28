package org.goldenport.i18n

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Feb. 23, 2022
 * @version Feb. 23, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StringFormatterSpec extends WordSpec with Matchers with GivenWhenThen {
  "StringFormatter" should {
    "display.shrink" which {
      "empty" in {
        StringFormatter.display.shrink("", 8, 3) should be("")
      }
      "1" in {
        StringFormatter.display.shrink("1", 8, 3) should be("1")
      }
      "12" in {
        StringFormatter.display.shrink("12", 8, 3) should be("12")
      }
      "1234567890" in {
        StringFormatter.display.shrink("1234567890", 8, 3) should be("123..890")
      }
      "123456789012" in {
        StringFormatter.display.shrink("123456789012", 8, 3) should be("123..012")
      }
    }
  }
}
