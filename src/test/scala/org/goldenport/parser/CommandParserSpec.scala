package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import CommandParser._

/*
 * @since   Oct. 12, 2019
 * @version Oct. 12, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CommandParserSpec extends WordSpec with Matchers with GivenWhenThen {
  "CommandParser" should {
    "CommandParser" which {
      "CommandParser" in {
        val parser = CommandParser.create("ab" -> "AB", "ac" -> "AC")
        parser("a") should be(Candidates.create("ab" -> "AB", "ac" -> "AC"))
      }
    }
  }
}
