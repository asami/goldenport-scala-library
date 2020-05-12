package org.goldenport.io

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.util.StringUtils

/*
 * @since   May.  4, 2020
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class StringInputStreamSpec extends WordSpec with Matchers with GivenWhenThen {
  val emptystring = ""
  val smallstring = "hello world!"
  val smallmultibytestring = "こんにちは 世界!"
  val largestring = {
    val sb = new StringBuilder()
    for (x <- 0 until 1000)
      for (i <- 0 until 10)
        sb.append(('0' + i).toChar)
//    println(sb.toString)
    sb.toString
  }
  val largemultibytestring = {
    val sb = new StringBuilder()
    for (x <- 0 until 1000)
      for (i <- 0 until 10)
        sb.append(_multi_number(i))
//    println(sb.toString)
    sb.toString
  }

  private def _multi_number(i: Int): Character = StringUtils.toKanjiCharacter(i)

  "BufferStrategy" should {
    "empty" in {
      val in = new StringInputStream(emptystring)
      val s = IoUtils.toText(in)
      s should equal(emptystring)
    }

    "typical" in {
      val in = new StringInputStream(smallstring)
      val s = IoUtils.toText(in)
      s should equal(smallstring)
    }

    "multibyte" in {
      val in = new StringInputStream(smallmultibytestring)
      val s = IoUtils.toText(in)
      s should equal(smallmultibytestring)
    }
  }

  "StreamStrategy" should {
    "typical" in {
      val in = new StringInputStream(largestring)
      val s = IoUtils.toText(in)
      s should equal(largestring)
    }

    "multibyte" in {
      val in = new StringInputStream(largemultibytestring)
      val s = IoUtils.toText(in)
      s should equal(largemultibytestring)
    }
  }
}
