package org.goldenport.io

import java.io.File
import java.net.URI
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

/*
 * @since   Mar. 17, 2026
 * @version Mar. 17, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class UriUtilsSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "getFile with base" should {
    "keep absolute scheme-less path absolute" in {
      val base = new File(".")
      val uri = new URI("/tmp/goldenport/uriutils-spec.txt")

      val file = UriUtils.getFile(base, uri).get

      file.isAbsolute should be(true)
      file.getPath should be("/tmp/goldenport/uriutils-spec.txt")
    }

    "resolve relative path against base directory" in {
      val base = new File("/tmp/goldenport")
      val uri = new URI("dir/file.txt")

      val file = UriUtils.getFile(base, uri).get

      file.isAbsolute should be(true)
      file.getPath should be(new File(base, "dir/file.txt").getPath)
    }
  }
}
