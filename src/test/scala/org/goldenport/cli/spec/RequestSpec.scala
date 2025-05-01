package org.goldenport.cli.spec

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}

/*
 * @since   Mar. 15, 2025
 *  version Mar. 16, 2025
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RequestSpec extends WordSpec with Matchers with GivenWhenThen {
  "RequestSpec" should {
    "buid" which {
      "typical" in {
        val req = Request(Parameter.property("a"), Parameter.argument("b"))
        val r = req.build(CliRequest("req"), List("-a", "A", "B"))
        r should be(CliRequest("req", CliArgument("b", "B", Parameter.argument("b")), CliProperty("a", "A", Parameter.property("a"))))
      }
    }
  }
}
