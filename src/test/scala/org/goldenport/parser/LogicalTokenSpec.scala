package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers._
import org.goldenport.util.DateTimeUtils

/*
 * @since   Jan. 19, 2021
 *  version Jan. 23, 2021
 * @version Feb. 13, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LogicalTokenSpec extends WordSpec with Matchers with GivenWhenThen {
  val context = LogicalTokens.Context.create()
  val jodajst = DateTimeUtils.jodajst
  val start = ParseLocation.start

  def token(p: LogicalToken) = TokenizerAcceptMatcher(p)

  case class TokenizerAcceptMatcher(o: LogicalToken) extends Matcher[Option[LogicalTokens]] {
    def apply(p: Option[LogicalTokens]) = p match {
      case Some(s) => s.tokens match {
        case x +: Seq() => MatchResult(x == o, s"$x was not equal to $o", s"$x was equal to $o")
        case x +: xs => MatchResult(false, s"Too many expressions: ${s}", "")
        case _ => MatchResult(false, "Empty expression", "")
      }
      case None => MatchResult(false, "Empty expression", "")
    }
  }

  "complex" should {
    "parse" which {
      "plain" in {
        val s = "1+5i"
        ComplexToken.accept(context, s, start) should token(ComplexToken(1, 5, start))
      }
    }
  }
  "rational" should {
    "parse" which {
      "plain" in {
        val s = "2/5"
        RationalToken.accept(context, s, start) should token(RationalToken(2, 5, start))
      }
    }
  }
  "range" should {
    "parse" which {
      "plain" in {
        val s = "1~3+1"
        RangeToken.accept(context, s, start) should token(RangeToken(1, 3, true, true, start))
      }
    }
  }
  "interval" should {
    "parse" which {
      "plain" in {
        val s = "1~3"
        IntervalToken.accept(context, s, start) should token(IntervalToken(1, 3, true, true, start))
      }
    }
  }
  "monthday" should {
    "parse" which {
      "plain" in {
        val s = "1-22"
        MonthDayToken.accept(context, s, start) should token(MonthDayToken(1, 22, start))
      }
    }
  }
  "datetime" should {
    "parse" which {
      "plain" in {
        val s = "2021-01-23T14:30:00+09"
        DateTimeToken.accept(context, s, start) should token(DateTimeToken(2021, 1, 23, 14, 30, 0, jodajst, start))
      }
    }
  }
  "localdatetime" should {
    "parse" which {
      "plain" in {
        val s = "2021-01-23T14:44:00"
        LocalDateTimeToken.accept(context, s, start) should token(LocalDateTimeToken(2021, 1, 23, 14, 44, 0, start))
      }
    }
  }
  "localdate" should {
    "parse" which {
      "plain" in {
        val s = "2021-01-23"
        LocalDateToken.accept(context, s, start) should token(LocalDateToken(2021, 1, 23, start))
      }
    }
  }
  "localtime" should {
    "parse" which {
      "plain" in {
        val s = "14:51:00"
        LocalTimeToken.accept(context, s, start) should token(LocalTimeToken(14, 51, 0, start))
      }
    }
  }
  "period" should {
    "parse" which {
      "plain" in {
        val s = "P5Y2M10D"
        PeriodToken.accept(context, s, start) should token(PeriodToken.yearMonthDay(5, 2, 10, start))
      }
      "hour" in {
        val s = "PT15H"
        PeriodToken.accept(context, s, start) should token(PeriodToken.hour(15, start))
      }
    }
  }
  "duration" should {
    "parse" which {
      "plain" in {
        val s = "DT15H"
        DurationToken.accept(context, s, start) should token(DurationToken.hour(15, start))
      }
    }
  }
  "datetimeinterval" should {
    "parse" which {
      "plain" in {
        val s = "2021-01-23T15:05:00+0900~"
        DateTimeIntervalToken.accept(context, s, start) should token(DateTimeIntervalToken.atOrAbove(2021, 1, 23, 15, 5, 0, jodajst, start))
      }
    }
  }
  "localdatetimeinterval" should {
    "parse" which {
      "plain" in {
        val s = "2021-01-23T15:07:00~"
        LocalDateTimeIntervalToken.accept(context, s, start) should token(LocalDateTimeIntervalToken.atOrAbove(2021, 1, 23, 15, 7, 0, start))
      }
    }
  }
}
