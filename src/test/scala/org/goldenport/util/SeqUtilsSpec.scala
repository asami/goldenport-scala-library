package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Feb. 17, 2016
 * @version Sep. 15, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SeqUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import SeqUtils._

  "mergeStringAnyWithOption" should {
    "typical" which {
      "typical" in {
        mergeStringAnyWithOption(
          Vector(
            "a" -> 1,
            "b" -> 2
          ),
          Vector(
            "a" -> 10,
            "c" -> 30
          )
        ) should be(Vector("a" -> 1, "b" -> 2, "c" -> 30))
      }
      "none" in {
        mergeStringAnyWithOption(
          Vector(
            "a" -> 1,
            "b" -> 2,
            "c" -> None
          ),
          Vector(
            "a" -> 10,
            "c" -> 30
          )
        ) should be(Vector("a" -> 1, "b" -> 2, "c" -> 30))
      }
    }
  }

  "split3V" should {
    "normal" which {
      "zero" in {
        val a = List()
        split3V((_: Int) == 3)(a) should be((Vector.empty, Vector.empty, Vector.empty))
      }
      "one left" in {
        val a = List(1)
        split3V((_: Int) == 3)(a) should be((Vector(1), Vector.empty, Vector.empty))
      }
      "two left" in {
        val a = List(1, 2)
        split3V((_: Int) == 3)(a) should be((Vector(1, 2), Vector.empty, Vector.empty))
      }
      "one center" in {
        val a = List(3)
        split3V((_: Int) == 3)(a) should be((Vector.empty, Vector(3), Vector.empty))
      }
      "two center" in {
        val a = List(1, 2)
        split3V((_: Int) == 1)(a) should be((Vector.empty, Vector(1), Vector(2)))
      }
      "two right" in {
        val a = List(1, 2)
        split3V((_: Int) == 2)(a) should be((Vector(1), Vector(2), Vector.empty))
      }
      "full" in {
        val a = List(1, 2, 3, 4, 5)
        split3V((_: Int) == 3)(a) should be((List(1, 2), List(3), List(4, 5)))
      }
    }
  }
}
