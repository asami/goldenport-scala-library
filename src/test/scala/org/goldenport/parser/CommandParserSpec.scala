package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import CommandParser._

/*
 * @since   Oct. 12, 2019
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CommandParserSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "CommandParser" should {
    "CommandParser" which {
      "CommandParser" in {
        val parser = CommandParser.create("ab" -> "AB", "ac" -> "AC")
        parser("a") should be(Candidates.create("ab" -> "AB", "ac" -> "AC"))
      }
    }
  }
}
