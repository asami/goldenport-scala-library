package org.goldenport.collection

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.context._

/*
 * @since   Jun. 12, 2025
 * @version Jun. 12, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NonEmptyVectorSpec extends WordSpec with Matchers with GivenWhenThen {
  "NonEmptyVector" when {
    "init" should {
      "one" in {
        val a = NonEmptyVector(1)
        assertThrows[ConclusionException] {
          a.tail
        }
      }
    }
  }
}
