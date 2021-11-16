package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.context._

/*
 * @since   Nov. 12, 2021
 * @version Nov. 13, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TriggerGuardSpec extends WordSpec with Matchers with GivenWhenThen {
  "TriggerGuard" should {
    "12/1" which {
      val dc = DateTimeContext.create(2021, 12, 1, 14, 55)
      "Day 31 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 31 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 15 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 15 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
    }
    "10/31" which {
      val dc = DateTimeContext.create(2021, 10, 31, 14, 55)
      "Day 31 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 31 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 15 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 15 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
    }
    "2/29 in leap year" which {
      val dc = DateTimeContext.create(2020, 2, 29, 14, 55)
      "Day 29 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 29 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 1 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 1 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
      "Day 15 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 15 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
    }
    "3/1 in leap year" which {
      val dc = DateTimeContext.create(2020, 3, 1, 14, 55)
      "Day 29 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 29 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
      "Day 1 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 1 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 15 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 15 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
    }
    "3/1 in normal year" which {
      val dc = DateTimeContext.create(2021, 3, 1, 14, 55)
      "Day 29 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 29 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 1 is availabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 1 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(true)
      }
      "Day 15 is unavailabe." in {
        val g = CronTriggerGuard.parse(dc, "55 14 15 * *").take
//        println(g)
        val x = g.getTimestamp(dc)
//        println(s"result: $x")
        x.isDefined should be(false)
      }
    }
  }
}
