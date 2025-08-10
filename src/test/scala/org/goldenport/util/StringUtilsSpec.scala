package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Jul.  2, 2025
 * @version Aug. 10, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StringUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import StringUtils._

  "StringUtils" when {
    "pathLastComponent" should {
      "absolute" in {
        pathLastComponent("/a/b/x.html") should be("x.html")
      }
      "relative" in {
        pathLastComponent("a/b/x.html") should be("x.html")
      }
      "leaf" in {
        pathLastComponent("x.html") should be("x.html")
      }
    }
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
    "camelToUnderscore" should {
      "with suffix" in {
        camelToUnderscore("AnalysisUpDown.png") should be("analysis_up_down.png")
      }
      "with uppercase suffix" in {
        camelToUnderscore("AnalysisUpDown.PNG") should be("analysis_up_down.png")
      }
      "pathname with uppercase suffix" in {
        camelToUnderscore("images/AnalysisUpDown.PNG") should be("images/analysis_up_down.png")
      }
    }
  }
}
