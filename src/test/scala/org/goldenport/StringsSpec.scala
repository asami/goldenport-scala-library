package org.goldenport

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Mar. 10, 2016
 * @version Mar. 10, 2016
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StringsSpec extends WordSpec with Matchers with GivenWhenThen {
  import Strings._

  "blankp" should {
    "null" in {
      val text: String = null
      blankp(text) should be(true)
    }
    "empty" in {
      blankp("") should be(true)
    }
    "blank" in {
      blankp(" ") should be(true)
    }
    "blank 5" in {
      blankp("     ") should be(true)
    }
    "one" in {
      blankp("a") should be(false)
    }
  }
  "notblankp" should {
    "null" in {
      val text: String = null
      notblankp(text) should be(false)
    }
    "empty" in {
      notblankp("") should be(false)
    }
    "blank" in {
      notblankp(" ") should be(false)
    }
    "blank 5" in {
      notblankp("     ") should be(false)
    }
    "one" in {
      notblankp("a") should be(true)
    }
  }
  "emptyp" should {
    "null" in {
      val text: String = null
      emptyp(text) should be(true)
    }
    "empty" in {
      emptyp("") should be(true)
    }
    "blank" in {
      emptyp(" ") should be(false)
    }
    "blank 5" in {
      emptyp("     ") should be(false)
    }
    "one" in {
      emptyp("a") should be(false)
    }
  }
  "notemptyp" should {
    "null" in {
      val text: String = null
      notemptyp(text) should be(false)
    }
    "empty" in {
      notemptyp("") should be(false)
    }
    "blank" in {
      notemptyp(" ") should be(true)
    }
    "blank 5" in {
      notemptyp("     ") should be(true)
    }
    "one" in {
      notemptyp("a") should be(true)
    }
  }
}
