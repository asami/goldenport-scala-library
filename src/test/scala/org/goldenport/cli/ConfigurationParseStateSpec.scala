package org.goldenport.cli

import scalaz._, Scalaz._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceMatchers
import ConfigurationParseState.StateFunctions._

/*
 * @since   May. 10, 2025
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ConfigurationParseStateSpec extends WordSpec with Matchers with GivenWhenThen with ConsequenceMatchers {
  "typical" should {
    "State" which {
      "empty" in {
        val program: State[ConfigurationParseState, (Option[Int], Option[Int])] =
          for {
            a <- getInt("one")
            b <- getInt("two")
          } yield (a, b)
        val (state, result) = program(ConfigurationParseState.empty)
        result should be((None, None))
      }
    }

    "StateT" which {
      "empty" in {
        val program: StateT[Consequence, ConfigurationParseState, (Option[Int], Option[Int])] =
          for {
            a <- cIntOption("one")
            b <- cIntOption("two")
          } yield (a, b)
//        val r: Consequence[(ConfigurationParseState, (Option[Int], Option[Int]))] = program(ConfigurationParseState.empty)
        val r = program(ConfigurationParseState.empty)
        r should be_success((ConfigurationParseState.empty, (none[Int], none[Int])))
      }
    }
  }
}
