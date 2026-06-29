package org.goldenport.cli.spec

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.cli.{Request => CliRequest, Switch => CliSwitch, Property => CliProperty, Argument => CliArgument}

/*
 * @since   Mar. 15, 2025
 *  version Mar. 16, 2025
 *  version Apr. 27, 2025
 *  version Aug. 16, 2025
 * @version Jun. 29, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RequestSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "RequestSpec" should {
    "buid" which {
      "typical" in {
        val req = Request(Parameter.property("a"), Parameter.argument("b"))
        val r = req.build(CliRequest("req"), List("-a", "A", "B"))
        r should be(CliRequest("req", CliArgument("b", "B", Parameter.argument("b")), CliProperty("a", "A", Parameter.property("a"))))
      }
      "int property" in {
        val param = Parameter.propertyInt("port")
        val req = Request(param)
        val r = req.build(CliRequest("req"), List("--port", "8080"))
        r should be(CliRequest(None, "req", Nil, Nil, List(CliProperty("port", 8080, param))))
        r.cInt(param).toOption should be(Some(8080))
      }
      "powertype property" in {
        val param = Parameter.propertyPowertypeOption(DataType, "datatype")
        val req = Request(param)
        val r = req.build(CliRequest("req"), List("--datatype", "string"))
        r.cPowertypeOption[DataType](param).toOption should be(Some(Some(XString)))
      }
      "invalid int property" in {
        val req = Request(Parameter.propertyInt("port"))
        an [Throwable] should be thrownBy {
          req.build(CliRequest("req"), List("--port", "not-int"))
        }
      }
      "invalid int property with positional argument" in {
        val req = Request(Parameter.argument("project"), Parameter.propertyInt("port"))
        an [Throwable] should be thrownBy {
          req.build(CliRequest("req"), List("/tmp/project", "--port", "not-int"))
        }
      }
      "unknown argument after metadata parse" in {
        val req = Request(Parameter.argument("project"), Parameter.property("name"))
        an [Throwable] should be thrownBy {
          req.buildStrict(CliRequest("req"), List("/tmp/project", "--name", "sample", "--unknown", "value"))
        }
      }
    }
  }
}
