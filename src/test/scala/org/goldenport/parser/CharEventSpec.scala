package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 26, 2018
 * @version Aug. 26, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CharEventSpec extends WordSpec with Matchers with GivenWhenThen {
  "CharEvent" should {
    "flat" in {
      val s = """a
b
c
"""
      val r = CharEvent.make(s)
      println(r)
    }
    "s-expression" in {
      val s = """(a b
c d)"""
      val r = CharEvent.make(s)
      println(r)
    }
  }
}
