package org.goldenport.collection

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._

/*
 * @since   Dec. 27, 2018
 * @version Dec. 27, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class VectorMapSpec extends WordSpec with Matchers with GivenWhenThen {
  "VectorMapSpec" should {
    val a = VectorMap("a" -> Vector("A"))
    val b = VectorMap("b" -> Vector("B"))
    val ab = VectorMap("a" -> Vector("A"), "b" -> Vector("B"))
    val aa = VectorMap("a" -> Vector("A", "A"))
    "typical" which {
      "update" in {
        val c = a update b
        c should be(ab)
      }
    }
    "monoid" which {
      "typical" in {
        val c = a |+| b
        c should be(ab)
      }
      "update" in {
        val c = a |+| a
        c should be(aa)
      }
    }
  }
}
