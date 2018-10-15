package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 26, 2018
 * @version Aug. 26, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class VectorUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import VectorUtils._

  "slinding2" should {
    "typical" in {
      sliding2("abc".toVector) should be(Vector(Vector('a', 'b'), Vector('b', 'c'), Vector('c')))
    }
    "zero" in {
      sliding2("".toVector) should be(Vector())
    }
    "one" in {
      sliding2("a".toVector) should be(Vector(Vector('a')))
    }
    "two" in {
      sliding2("ab".toVector) should be(Vector(Vector('a', 'b'), Vector('b')))
    }
  }

  "slinding3" should {
    "typical" in {
      sliding3("abc".toVector) should be(Vector(Vector('a', 'b', 'c'), Vector('b', 'c'), Vector('c')))
    }
    "zero" in {
      sliding3("".toVector) should be(Vector())
    }
    "one" in {
      sliding3("a".toVector) should be(Vector(Vector('a')))
    }
    "two" in {
      sliding3("ab".toVector) should be(Vector(Vector('a', 'b'), Vector('b')))
    }
  }
}
