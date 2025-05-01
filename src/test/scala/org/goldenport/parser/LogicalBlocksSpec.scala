package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Sep. 22, 2018
 *  version Oct. 25, 2018
 *  version Feb.  2, 2019
 * @version Feb.  8, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalBlocksSpec extends WordSpec with Matchers with GivenWhenThen {
  "LogicalBlocks" should {
    "operation" which {
      "+" in {
        val r = LogicalBlocks.create("a") + LogicalBlocks.empty
        r should be(LogicalBlocks.create("a"))
      }
    }
    "blocks" which {
      "one block" in {
        val s = """a
b
c
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(LogicalParagraph(LogicalLines(
          LogicalLine("a", ParseLocation.start),
          LogicalLine("b", ParseLocation(2, 1)),
          LogicalLine("c", ParseLocation(3, 1))))))
      }
      "two blocks" in {
        val s = """abc
def
fhi

xyz
wxy
vwx
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalParagraph(LogicalLines.start("abc", "def", "fhi")),
          LogicalParagraph(LogicalLines.start(5, "xyz", "wxy", "vwx"))))
      }
      "single quote in head" in {
        val s = """'"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(LogicalParagraph(LogicalLines.start("'"))))
      }
      "single quote" in {
        val s = """a'b"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(LogicalParagraph(LogicalLines.start("a'b"))))
      }
    }
    "section" which {
      "one section" in {
        val s = """* div1
content1
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalSection(
            "div1",
            LogicalBlocks(
              LogicalParagraph("content1", ParseLocation(2, 1))
            ),
            "*",
            ParseLocation.start
          )
        ))
      }
      "one section with space" in {
        val s = """* div1

content1
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalSection("div1", LogicalBlocks(
            LogicalParagraph("content1", ParseLocation(3, 1))
          ), "*", ParseLocation.start)))
      }
      "two sections" in {
        val s = """* div1

content1

* div2

content2
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalSection("div1", LogicalBlocks(
            LogicalParagraph("content1", ParseLocation(3, 1))
          ), "*", ParseLocation.start),
          LogicalSection("div2", LogicalBlocks(
            LogicalParagraph("content2", ParseLocation(7, 1))
          ), "*", ParseLocation(5, 1))
        ))
      }
      "nest" in {
        val s = """* div1

content1

** div11

content11

*** div111

content111
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalSection("div1", LogicalBlocks(
            LogicalParagraph("content1", ParseLocation(3, 1)),
            LogicalSection("div11", LogicalBlocks(
              LogicalParagraph("content11", ParseLocation(7, 1)),
              LogicalSection("div111", LogicalBlocks(
                LogicalParagraph("content111", ParseLocation(11, 1))
              ), "*", ParseLocation(9, 1))
            ), "*", ParseLocation(5, 1))
          ), "*", ParseLocation.start)
        ))
      }
      "nest up down" in {
        val s = """* div1

content1

** div11

content11

*** div111

content111

** div12

content12

*** div121

content121
"""
        val r = LogicalBlocks.parseDebug(s)
        r should be(LogicalBlocks(
          LogicalSection("div1", LogicalBlocks(
            LogicalParagraph("content1", ParseLocation(3, 1)),
            LogicalSection("div11", LogicalBlocks(
              LogicalParagraph("content11", ParseLocation(7, 1)),
              LogicalSection("div111", LogicalBlocks(
                LogicalParagraph("content111", ParseLocation(11, 1))
              ), "*", ParseLocation(9, 1))
            ), "*", ParseLocation(5, 1)),
            LogicalSection("div12", LogicalBlocks(
              LogicalParagraph("content12", ParseLocation(15, 1)),
              LogicalSection("div121", LogicalBlocks(
                LogicalParagraph("content121", ParseLocation(19, 1))
              ), "*", ParseLocation(17, 1))
            ), "*", ParseLocation(13, 1))
          ), "*", ParseLocation.start)
        ))
      }
    }
  }
}
