package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Oct. 14, 2018
 * @version Oct. 15, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class JsonParserSpec extends WordSpec with Matchers with GivenWhenThen {
  "JsonParser" should {
    "nest" in {
      val s = """{"user": {"name":"taro", "city":"yokohama"}}"""
      // See org.goldenport.sexpr.script.ScriptSpec
    }
  }
}
