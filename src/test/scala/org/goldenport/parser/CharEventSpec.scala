package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 26, 2018
 * @version Sep.  2, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CharEventSpec extends WordSpec with Matchers with GivenWhenThen {
  "CharEvent" should {
    "flat" in {
      val s = """a
b
c
"""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('a', Some('\n'), Some('b'), ParseLocation(1, 1)),
        CharEvent('\n', Some('b'), Some('\n'), ParseLocation(1, 2)),
        CharEvent('b', Some('\n'), Some('c'), ParseLocation(2, 1)),
        CharEvent('\n', Some('c'), Some('\n'), ParseLocation(2, 2)),
        CharEvent('c', Some('\n'), None, ParseLocation(3, 1)),
        CharEvent('\n', None, None, ParseLocation(3, 2))
      ))
    }
    "s-expression" in {
      val s = """(a b
c d)"""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('(', Some('a'), Some(' '), ParseLocation(1, 1)),
        CharEvent('a', Some(' '), Some('b'), ParseLocation(1, 2)),
        CharEvent(' ', Some('b'), Some('\n'), ParseLocation(1, 3)),
        CharEvent('b', Some('\n'), Some('c'), ParseLocation(1, 4)),
        CharEvent('\n', Some('c'), Some(' '), ParseLocation(1, 5)),
        CharEvent('c', Some(' '), Some('d'), ParseLocation(2, 1)),
        CharEvent(' ', Some('d'), Some(')'), ParseLocation(2, 2)),
        CharEvent('d', Some(')'), None, ParseLocation(2, 3)),
        CharEvent(')', None, None, ParseLocation(2, 4))
      ))
    }
  }
}
