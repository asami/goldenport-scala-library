package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import spire.math.Rational

/*
 * @since   Mar.  6, 2022
 * @version Mar.  6, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PathNameSpec extends WordSpec with Matchers with GivenWhenThen {
  import Urn._

  "PathName" should {
    "leaf" which {
      "leaf" in {
        val pn = PathName("/a/b/c.html")
        pn.leaf should be("c.html")
      }
    }
    "leafBody" which {
      "leafBody" in {
        val pn = PathName("/a/b/c.html")
        pn.leafBody should be("c")
      }
    }
    "body" which {
      "body" in {
        val pn = PathName("/a/b/c.html")
        pn.body should be("/a/b/c")
      }
    }
    "suffix" which {
      "suffix" in {
        val pn = PathName("/a/b/c.html")
        pn.suffix should be("html")
      }
      "upper case" in {
        val pn = PathName("/a/b/c.HTML")
        pn.suffix should be("html")
      }
    }
    "trunk" which {
      "trunk" in {
        val pn = PathName("/a/b/c.html")
        pn.trunk should be("/a/b")
      }
    }
    "getTrunk" which {
      "getTrunk" in {
        val pn = PathName("/a/b/c.html")
        pn.getTrunk should be(Some(PathName("/a/b")))
      }
    }
    "getChild" which {
      "getChild" in {
        val pn = PathName("/a/b/c.html")
        pn.getChild should be(Some(PathName("b/c.html")))
      }
    }
  }
}
