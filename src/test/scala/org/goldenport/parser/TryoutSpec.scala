package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
// import org.joda.time._
import org.scalatest.matchers._
import org.goldenport.util.DateTimeUtils

/*
 * @since   Jan.  1, 2019
 *  version Feb.  9, 2019
 *  version Mar. 10, 2019
 *  version Apr. 13, 2019
 *  version Feb. 13, 2021
 *  version Apr. 26, 2021
 * @version Oct.  1, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  "script" should {
    val config = LogicalLines.Config.script
    def parse(p: String) = LogicalLines.parse(config, p)

    "normal lines" in {
      val s = """a
b
c
"""
      val r = parse(s)
      r should be(LogicalLines.start("a", "b", "c"))
    }
    "one syntax error" ignore {
      val s = """'"""
      an [ParseSyntaxErrorException] should be thrownBy parse(s)
    }
    "double quote" which {
      "new line" in {
      val s = """a "
b" c
"""
      val r = parse(s)
      r should be(LogicalLines.start("""a "
b" c"""))
      }
      "one" ignore {
        val s = "\"\"\"a \"x \"\"\""
        val r = parse(s)
        r should be(LogicalLines.start("\"\"\"a \"x \"\"\""))
      }
      "stick" ignore {
        val s = "\"\"\"a\"x\"\"\"\""
        val r = parse(s)
        r should be(LogicalLines.start("\"\"\"a\"x\"\"\"\""))
      }
    }
    "lisp" ignore {
      val conf = LogicalLines.Config.lisp
      def parselisp(p: String) = LogicalLines.parse(conf, p)

      "single quote" in {
        val s = """'a"""
        val r = parselisp(s)
        r should be(LogicalLines.start("""'a"""))
      }
    }
    "s-expression" which {
      "one line" ignore {
        val s = """(a b c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("(a b c d)"))
      }
      "tow lines" in {
        val s = """(a b
c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("(a b\nc d)"))
      }
      "double quote" ignore {
        val s = """(a b "s
" c d)"""
        val r = parse(s)
        r should be(LogicalLines.start("""(a b "s
" c d)"""))
      }
    }
  }
  // val context = LogicalTokens.Context.create()
  // val jodajst = DateTimeUtils.jodajst
  // val start = ParseLocation.start
  // def token(p: LogicalToken) = TokenizerAcceptMatcher(p)
  // case class TokenizerAcceptMatcher(o: LogicalToken) extends Matcher[Option[LogicalTokens]] {
  //   def apply(p: Option[LogicalTokens]) = p match {
  //     case Some(s) => s.tokens match {
  //       case x +: Seq() => MatchResult(x == o, s"$x was not equal to $o", s"$x was equal to $o")
  //       case x +: xs => MatchResult(false, s"Too many expressions: ${s}", "")
  //       case _ => MatchResult(false, "Empty expression", "")
  //     }
  //     case None => MatchResult(false, "Empty expression", "")
  //   }
  // }

  // "period" should {
  //   "parse" which {
  //     "plain" in {
  //       val s = "P5Y2M10D"
  //       PeriodToken.accept(context, s, start) should token(PeriodToken.yearMonthDay(5, 2, 10, start))
  //     }
  //     "hour" in {
  //       val s = "PT15H"
  //       PeriodToken.accept(context, s, start) should token(PeriodToken.hour(15, start))
  //     }
  //   }
  // }
  // "duration" should {
  //   "parse" which {
  //     "plain" in {
  //       val s = "DT15H"
  //       DurationToken.accept(context, s, start) should token(DurationToken.hour(15, start))
  //     }
  //   }
  // }

  // "token" should {
    // "json" which {
    //   "number value" in {
    //     val s = """{"a":1}"""
    //     val r = LogicalTokens.parse(s)
    //     r should be(LogicalTokens(
    //       JsonParser.JsonToken(s, ParseLocation.start)
    //     ))
    //   }
    // }
    // }
//   "LogicalBlocks" should {
//     "easytext" which {
//       val config = LogicalBlocks.Config.easytext
//       def parse(p: String): LogicalBlocks = LogicalBlocks.parse(config, p)

//       "src" in {
//         val s = """
// ```
// a
// ```
// """
//         val r = parse(s)
//         r should be (LogicalBlocks(
//           Vector(
//             LogicalParagraph(LogicalLines(LogicalLine.empty)),
//             LogicalVerbatim(
//               LogicalBlock.RawBackquoteMark(LogicalLine("```", ParseLocation.start)),
//               LogicalLines("a", ParseLocation(3, 1)),
//               Some(ParseLocation.start)
//             )
//             // ),
//             // LogicalParagraph(LogicalLines(LogicalLine("")))
//           )
//         ))
//       }
//       "begin_src" in {
//               val s = """
// #+begin_src console
// a
// #+end_src
// """
//         val r = parse(s)
//         r should be (LogicalBlocks(
//           Vector(
//             LogicalVerbatim(
//               LogicalBlock.RawBackquoteMark(LogicalLine("```", ParseLocation.start)),
//               LogicalLines("a"),
//               None
//             )
//           )
//         ))
//       }
}
