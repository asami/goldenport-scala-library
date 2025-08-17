package org.goldenport.cli.spec

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}

/*
 * @since   Mar. 16, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParameterSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  import Parameter.ParseState

  "ParameterSpec" should {
    "parsePropertySwitch" which {
      "typical" in {
        val param = Parameter.property("a")
        val s = ParseState.create("req", Vector("-a", "A", "b"))
        val r = param.parsePropertySwitch(s)
        r should be(
          ParseState(
            CliRequest(None, "req", Nil, Nil, List(CliProperty("a", "A", param))),
            Vector("b")))
      }
    }
  }
}
