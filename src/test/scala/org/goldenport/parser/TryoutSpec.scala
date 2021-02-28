package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import org.goldenport.parser._

/*
 * @since   Jan.  1, 2019
 *  version Feb.  9, 2019
 *  version Mar. 10, 2019
 *  version Apr. 13, 2019
 * @version Feb. 13, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
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
  "LogicalBlocks" should {
    "easytext" which {
      val config = LogicalBlocks.Config.easytext
      def parse(p: String): LogicalBlocks = LogicalBlocks.parse(config, p)

      "src" in {
        val s = """
```
a
```
"""
        val r = parse(s)
        r should be (LogicalBlocks(
          Vector(
            LogicalParagraph(LogicalLines(LogicalLine.empty)),
            LogicalVerbatim(
              LogicalBlock.RawBackquoteMark(LogicalLine("```", ParseLocation.start)),
              LogicalLines("a", ParseLocation(3, 1)),
              Some(ParseLocation.start)
            )
            // ),
            // LogicalParagraph(LogicalLines(LogicalLine("")))
          )
        ))
      }
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
  }
}
