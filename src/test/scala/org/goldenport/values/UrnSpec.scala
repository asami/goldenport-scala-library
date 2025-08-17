package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._

/*
 * @since   Oct. 31, 2017
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class UrnSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  import Urn._

  "urn" should {
    "urn" in {
      val urn = Urn("urn:prefer:free:/web/banner")
      urn.nid should be("prefer")
      urn.module should be("free")
      urn.submodule should be("/web/banner")
    }
  }
}
