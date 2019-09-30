package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Sep. 22, 2019
 * @version Sep. 23, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class InterpolationParserSpec extends WordSpec with Matchers with GivenWhenThen {
  "InterpolationParser" should {
    "empty" in {
      InterpolationParser.parse("") should be(Vector.empty)
    }
    "no expression" in {
      InterpolationParser.parse("abc") should be(Vector(Left("abc")))
    }
    "expression only" in {
      InterpolationParser.parse("${xyz}") should be(Vector(Right(ScriptToken("xyz"))))
    }
    "handle an expression only string using math expresson" in {
      InterpolationParser.parse("${1 + 2}") should be(Vector(Right(ScriptToken("1 + 2"))))
    }
    "one expression" in {
      InterpolationParser.parse("abc ${xyz} def") should be(Vector(Left("abc "), Right(ScriptToken("xyz")), Left(" def"))) // XXX Unavailable ParseLocation
    }
    "two expressions" in {
      InterpolationParser.parse("abc ${xyz} def ${XYZ} ghi") should be(Vector(Left("abc "), Right(ScriptToken("xyz")), Left(" def "), Right(ScriptToken("XYZ")), Left(" ghi")))
    }
    "space hnadling" in {
      InterpolationParser.parse("abc  ${xyz} def  def ${XYZ}  ghi") should be(Vector(Left("abc  "), Right(ScriptToken("xyz")), Left(" def  def "), Right(ScriptToken("XYZ")), Left("  ghi")))
    }
    "handle a script expression" in {
      InterpolationParser.parse("abc ${a + 10} def") should be(Vector(Left("abc "), Right(ScriptToken("a + 10")), Left(" def")))
    }
  }
  // "tryout" should {
  //   "handle an expression only string using math expresson" in {
  //     InterpolationParser.parse("${1 + 2}") should be(Vector(Right(ScriptToken("1 + 2"))))
  //   }
  // }
}
