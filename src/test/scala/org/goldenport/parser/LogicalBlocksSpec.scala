package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Sep. 22, 2018
 * @version Sep. 24, 2018
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
        r should be(LogicalBlocks(LogicalParagraph(LogicalLines("a", "b", "c"))))
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
        r should be(LogicalBlocks(LogicalParagraph(LogicalLines("abc", "def", "fhi")), LogicalParagraph(LogicalLines("xyz", "wxy", "vwx"))))
      }
    }
    "section" which {
      "one section" in {
        val s = """* div1
content1
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(LogicalSection("div1", LogicalBlocks(LogicalParagraph("content1")))))
      }
      "one section with space" in {
        val s = """* div1

content1
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(LogicalSection("div1", LogicalBlocks(LogicalParagraph("content1")))))
      }
      "two sections" in {
        val s = """* div1

content1

* div2

content2
"""
        val r = LogicalBlocks.parse(s)
        r should be(LogicalBlocks(
          LogicalSection("div1", LogicalBlocks(LogicalParagraph("content1"))),
          LogicalSection("div2", LogicalBlocks(LogicalParagraph("content2")))
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
            LogicalParagraph("content1"),
            LogicalSection("div11", LogicalBlocks(
              LogicalParagraph("content11"),
              LogicalSection("div111", LogicalBlocks(
                LogicalParagraph("content111")
              ))
            ))
          ))
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
            LogicalParagraph("content1"),
            LogicalSection("div11", LogicalBlocks(
              LogicalParagraph("content11"),
              LogicalSection("div111", LogicalBlocks(
                LogicalParagraph("content111")
              ))
            )),
            LogicalSection("div12", LogicalBlocks(
              LogicalParagraph("content12"),
              LogicalSection("div121", LogicalBlocks(
                LogicalParagraph("content121")
              ))
            ))
          ))
        ))
      }
    }
  }
}
