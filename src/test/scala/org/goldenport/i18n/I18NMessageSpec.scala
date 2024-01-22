package org.goldenport.i18n

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Dec.  8, 2022
 * @version Dec.  8, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class I18NMessageSpec extends WordSpec with Matchers with GivenWhenThen {
  "I18NMessage" should {
    "I18NMessage" which {
      "I18NString" in {
        val i = """{"a": "b"}"""
        val s = I18NString(i)
        val m = s.toI18NMessage
        val r = m.en
        println(r)
      }
    }
  }
}
