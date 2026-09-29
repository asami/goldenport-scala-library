package org.goldenport.collection

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/*
 * @since   Sep. 26, 2026
 * @version Sep. 26, 2026
 */
@RunWith(classOf[JUnitRunner])
class TreeMapSpec extends AnyWordSpec with Matchers with GivenWhenThen with ScalaCheckDrivenPropertyChecks {
  "TreeMap composition" should {
    "retain entries from two nonempty maps without a class cast" in {
      forAll { (left: Int, right: Int) =>
        Given("two separately built nonempty maps")
        val first = TreeMap.create("first" -> left)
        val second = TreeMap.create("second" -> right)

        When("the maps are composed")
        val combined = first + second

        Then("both entries remain available")
        combined.get("first") shouldBe Some(left)
        combined.get("second") shouldBe Some(right)
        combined.size shouldBe 2
      }
    }
  }
}
