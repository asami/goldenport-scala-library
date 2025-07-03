package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Jul.  2, 2025
 * @version Jul.  2, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StringUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import StringUtils._

  "StringUtils" when {
    "pathContainer" should {
      "leaf with suffix" in {
        pathContainer("/a/b/x.html") should be("/a/b")
      }
      "container end" in {
        pathContainer("/a/b/") should be("/a")
      }
      "leaf withdout suffix" in {
        pathContainer("/a/b") should be("/a")
      }
      "leaf with suffix relative" in {
        pathContainer("a/b/x.html") should be("a/b")
      }
      "container end relative" in {
        pathContainer("a/b/") should be("a")
      }
      "leaf withdout suffix relative" in {
        pathContainer("a/b") should be("a")
      }
    }
    "makePathContainer" should {
      "leaf with suffix" in {
        makePathContainer("/a/b/x.html") should be("/a/b/")
      }
      "container end" in {
        makePathContainer("/a/b/") should be("/a/b/")
      }
      "leaf withdout suffix" in {
        makePathContainer("/a/b") should be("/a/b/")
      }
      "leaf with suffix relative" in {
        makePathContainer("a/b/x.html") should be("a/b/")
      }
      "container end relative" in {
        makePathContainer("a/b/") should be("a/b/")
      }
      "leaf withdout suffix relative" in {
        makePathContainer("a/b") should be("a/b/")
      }
    }
    "makePathContainerBody" should {
      "leaf with suffix" in {
        makePathContainerBody("/a/b/x.html") should be("/a/b")
      }
      "container end" in {
        makePathContainerBody("/a/b/") should be("/a/b")
      }
      "leaf withdout suffix" in {
        makePathContainerBody("/a/b") should be("/a/b")
      }
      "leaf with suffix relative" in {
        makePathContainerBody("a/b/x.html") should be("a/b")
      }
      "container end relative" in {
        makePathContainerBody("a/b/") should be("a/b")
      }
      "leaf withdout suffix relative" in {
        makePathContainerBody("a/b") should be("a/b")
      }
    }
    "makePathContainerRelativeBody" should {
      "leaf with suffix" in {
        makePathContainerRelativeBody("/a/b/x.html") should be("a/b")
      }
      "container end" in {
        makePathContainerRelativeBody("/a/b/") should be("a/b")
      }
      "leaf withdout suffix" in {
        makePathContainerRelativeBody("/a/b") should be("a/b")
      }
      "leaf with suffix relative" in {
        makePathContainerRelativeBody("a/b/x.html") should be("a/b")
      }
      "container end relative" in {
        makePathContainerRelativeBody("a/b/") should be("a/b")
      }
      "leaf withdout suffix relative" in {
        makePathContainerRelativeBody("a/b") should be("a/b")
      }
    }
  }
}
