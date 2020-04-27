package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

/*
 * @since   Aug. 26, 2018
 *  version Sep.  2, 2018
 *  version Jan.  3, 2019
 * @version Feb. 29, 2020
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
        CharEvent('a', Some('\n'), Some('b'), Some('\n'), ParseLocation(1, 1)),
        CharEvent('\n', Some('b'), Some('\n'), Some('c'), ParseLocation(1, 2)),
        CharEvent('b', Some('\n'), Some('c'), Some('\n'), ParseLocation(2, 1)),
        CharEvent('\n', Some('c'), Some('\n'), None, ParseLocation(2, 2)),
        CharEvent('c', Some('\n'), None, None, ParseLocation(3, 1)),
        CharEvent('\n', None, None, None, ParseLocation(3, 2))
      ))
    }
    "s-expression" in {
      val s = """(a b
c d)"""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('(', Some('a'), Some(' '), Some('b'), ParseLocation(1, 1)),
        CharEvent('a', Some(' '), Some('b'), Some('\n'), ParseLocation(1, 2)),
        CharEvent(' ', Some('b'), Some('\n'), Some('c'), ParseLocation(1, 3)),
        CharEvent('b', Some('\n'), Some('c'), Some(' '), ParseLocation(1, 4)),
        CharEvent('\n', Some('c'), Some(' '), Some('d'), ParseLocation(1, 5)),
        CharEvent('c', Some(' '), Some('d'), Some(')'), ParseLocation(2, 1)),
        CharEvent(' ', Some('d'), Some(')'), None, ParseLocation(2, 2)),
        CharEvent('d', Some(')'), None, None, ParseLocation(2, 3)),
        CharEvent(')', None, None, None, ParseLocation(2, 4))
      ))
    }
    "raw string" in {
      val s = "\"\"\"a\"\"\""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('"', Some('"'), Some('"'), Some('a'), ParseLocation(1, 1)),
        CharEvent('"', Some('"'), Some('a'), Some('"'), ParseLocation(1, 2)),
        CharEvent('"', Some('a'), Some('"'), Some('"'), ParseLocation(1, 3)),
        CharEvent('a', Some('"'), Some('"'), Some('"'), ParseLocation(1, 4)),
        CharEvent('"', Some('"'), Some('"'), None, ParseLocation(1, 5)),
        CharEvent('"', Some('"'), None, None, ParseLocation(1, 6)),
        CharEvent('"', None, None, None, ParseLocation(1, 7))
      ))
    }
    "raw string 2" in {
      val s = "\"\"\"\"\"\""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 1)),
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 2)),
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 3)),
        CharEvent('"', Some('"'), Some('"'), None, ParseLocation(1, 4)),
        CharEvent('"', Some('"'), None, None, ParseLocation(1, 5)),
        CharEvent('"', None, None, None, ParseLocation(1, 6))
      ))
    }
    "raw string 3" in {
      val s = "\"\"\"\"\"\"\""
      val r = CharEvent.make(s)
      r should be(Vector(
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 1)),
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 2)),
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 3)),
        CharEvent('"', Some('"'), Some('"'), Some('"'), ParseLocation(1, 4)),
        CharEvent('"', Some('"'), Some('"'), None, ParseLocation(1, 5)),
        CharEvent('"', Some('"'), None, None, ParseLocation(1, 6)),
        CharEvent('"', None, None, None, ParseLocation(1, 7))
      ))
    }
  }
}
