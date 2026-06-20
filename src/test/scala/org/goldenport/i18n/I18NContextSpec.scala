package org.goldenport.i18n

import java.util.Locale
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._

/*
 * @since   Jun. 21, 2026
 * @version Jun. 21, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class I18NContextSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "I18NContext" should {
    "ResourceBundle messages" which {
      "load UTF-8 properties and format messages" in {
        Given("a UTF-8 ResourceBundle loaded through I18NContext")
        val bundle = I18NContext.loadResourceBundle(
          "org.goldenport.i18n.TestMessages",
          Locale.JAPAN,
          I18NContext.ResourceBundleConfig.englishFallback
        )
        val context = I18NContext.ja.copy(resourceBundle = bundle)

        When("messages are read by key")
        val title = context.message("title")
        val hello = context.message("hello", "太郎")
        val fallback = context.message("utf8")
        val missing = context.message("missing.key")

        Then("locale values and base fallback values are returned")
        title shouldBe "ダッシュボード"
        hello shouldBe "こんにちは 太郎"
        fallback shouldBe "Café"
        missing shouldBe "missing.key"
      }
    }
  }
}
