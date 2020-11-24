package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.parser.ParseResultMatchers

/*
 * @since   Sep. 28, 2020
 *  version Sep. 28, 2020
 * @version Oct. 12, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NumberRangeSpec extends WordSpec with Matchers with GivenWhenThen with ParseResultMatchers {
  def parselabel(p: String): Option[Number] = Option(p) collect {
    case "北海道" => 1
    case "東京都" => 13
    case "福岡県" => 40
    case "沖縄県" => 47
  }

  "isMatch" should {
    "int" in {
      val range = NumberRange.create("1~5")
      println(range)
      range.isMatch(0) should be (false)
      range.isMatch(1) should be (true)
      range.isMatch(3) should be (true)
      range.isMatch(5) should be (true)
      range.isMatch(6) should be (false)
    }
    "float" in {
      val range = NumberRange.create("1~5")
      range.isMatch(0.5) should be (false)
      range.isMatch(1) should be (true)
      range.isMatch(3) should be (true)
      range.isMatch(3.5) should be (false)
      range.isMatch(4) should be (true)
      range.isMatch(5) should be (true)
      range.isMatch(6.1) should be (false)
    }
  }
  "parse" should {
    "EnumRange" in {
      val s = "1,2,3"
      val r = NumberRange.create(s)
      r should be(EnumRange(1, 2, 3))
    }
  }
  "parseLabel" should {
    "EnumRange" which {
      "enum" in {
        val s = "北海道,東京都,沖縄県"
        val r = NumberRange.parseLabel(parselabel, s)
        r should parse_object(EnumRange(1, 13, 47))
      }
      "interval and enum" in {
        val s = "北海道~東京都,福岡県~沖縄県"
        val r = NumberRange.parseLabel(parselabel, s)
        r should parse_object(CompositeRange(RepeatRange(1, 13), RepeatRange(40, 47)))
      }
    }
  }
  // "tryout" should {
  //   "float" in {
  //     val range = NumberRange.create("1~5")
  //     range.isMatch(6.1) should be (false)
  //   }
  // }
}
