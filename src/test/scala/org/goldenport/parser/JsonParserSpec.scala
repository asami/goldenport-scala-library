package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._

/*
 * @since   Oct. 14, 2018
 *  version Oct. 15, 2018
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class JsonParserSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "JsonParser" should {
    "nest" in {
      val s = """{"user": {"name":"taro", "city":"yokohama"}}"""
      // See org.goldenport.sexpr.script.ScriptSpec
    }
  }
}
