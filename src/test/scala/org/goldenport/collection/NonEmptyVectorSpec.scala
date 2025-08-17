package org.goldenport.collection

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.context._

/*
 * @since   Jun. 12, 2025
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NonEmptyVectorSpec extends AnyWordSpec with Matchers with GivenWhenThen {
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
