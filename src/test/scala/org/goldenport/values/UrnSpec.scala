package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Oct. 31, 2017
 * @version Oct. 31, 2017
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class UrnSpec extends WordSpec with Matchers with GivenWhenThen {
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
