package org.goldenport.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.joda.time._
import org.goldenport.parser._

/*
 * @since   Jan.  1, 2019
 *  version Feb.  9, 2019
 * @version Mar. 10, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  "token" should {
    "script" which {
//       "typical" in {
//         val s = """${a + b}"""
//         val r = LogicalTokens.parse(s)
//         r should be(LogicalTokens(
//           ScriptToken("a + b", ParseLocation.start)
//         ))
//       }
//       "short form" in {
//         val s = """$a+b"""
//         val r = LogicalTokens.parse(s)
//         r should be(LogicalTokens(
//           ScriptToken("a+b", ParseLocation.start)
//         ))
//       }
//       "multi line" in {
//         val s = """${
// val c = a + b
// print(c)
// }"""
//         val r = LogicalTokens.parse(s)
//         r should be(LogicalTokens(
//           ScriptToken("""
// val c = a + b
// print(c)
// """, ParseLocation.start)
//         ))
//       }
      // "nest curly bracket" in {
      //   val s = """${{"a": "b"}}"""
      //   val r = LogicalTokens.parse(s)
      //   r should be(LogicalTokens(
      //     ScriptToken("""{"a": "b"}""", ParseLocation.start)
      //   ))
      // }
//       "extra mark" in {
//         val s = """${{{
// val c = a + b
// print(c)
// }}}"""
//         val r = LogicalTokens.parse(s)
//         r should be(LogicalTokens(
//           ScriptToken("""
// val c = a + b
// print(c)
// """, ParseLocation.start)
//         ))
//       }
    }
  }

  // "token" should {
  //   "expression" which {
  //     "numerical expression" in {
  //       val s = """a+b"""
  //       val r = LogicalTokens.parse(s)
  //       r should be(LogicalTokens(
  //         ExpressionToken(s, ParseLocation.start)
  //       ))
  //     }
  //   }
  // }
  // LogicalTokensSpec
  // "token" should {
  //   "number" which {
  //     "localdate" in {
  //       val s = "2018-09-09"
  //       val r = LogicalTokens.parse(s)
  //       r should be(LogicalTokens(
  //         LocalDateToken(LocalDate.parse(s), ParseLocation.init)
  //       ))
  //     }
  //   }
  //   "jxpath" which {
  //     "typical" in {
  //       val s = """/a/b/c[id='z']"""
  //       val r = LogicalTokens.parse(s)
  //       r should be(LogicalTokens(
  //         PathToken(s, ParseLocation.init)
  //       ))
  //     }
  //   }
  //   "double quote in raw string" in {
  //     val s = "\"\"\"\"\"\"\""
  //     val r = LogicalTokens.parseDebug(s)
  //     r should be(LogicalTokens(
  //       RawStringToken("\"", ParseLocation.init)
  //     ))
  //   }
  //   "double quote in raw string 2" in {
  //     val s = "\"\"\"\"\"\"\"\""
  //     val r = LogicalTokens.parseDebug(s)
  //     r should be(LogicalTokens(
  //       RawStringToken("\"\"", ParseLocation.init)
  //     ))
  //   }
  // }
  // LogicalBlocksSpec
//   "LogicalBlocks" should {
//     "section" which {
//       "one section" in {
//         val s = """* div1
// content1
// """
//         val r = LogicalBlocks.parse(s)
//         r should be(LogicalBlocks(
//           LogicalSection("div1", LogicalBlocks(
//             LogicalParagraph("content1", ParseLocation(2, 1))
//           ), ParseLocation.start)
//         ))
//       }
//       "one section with space" in {
//         val s = """* div1

// content1
// """
//         val r = LogicalBlocks.parse(s)
//         r should be(LogicalBlocks(LogicalSection("div1", LogicalBlocks(LogicalParagraph("content1")))))
//       }
//       "two sections" in {
//         val s = """* div1

// content1

// * div2

// content2
// """
//         val r = LogicalBlocks.parse(s)
//         r should be(LogicalBlocks(
//           LogicalSection("div1", LogicalBlocks(LogicalParagraph("content1"))),
//           LogicalSection("div2", LogicalBlocks(LogicalParagraph("content2")))
//         ))
//       }
//       "nest" in {
//         val s = """* div1

// content1

// ** div11

// content11

// *** div111

// content111
// """
//         val r = LogicalBlocks.parse(s)
//         r should be(LogicalBlocks(
//           LogicalSection("div1", LogicalBlocks(
//             LogicalParagraph("content1"),
//             LogicalSection("div11", LogicalBlocks(
//               LogicalParagraph("content11"),
//               LogicalSection("div111", LogicalBlocks(
//                 LogicalParagraph("content111")
//               ))
//             ))
//           ))
//         ))
//       }
//       "nest up down" in {
//         val s = """* div1

// content1

// ** div11

// content11

// *** div111

// content111

// ** div12

// content12

// *** div121

// content121
// """
//         val r = LogicalBlocks.parseDebug(s)
//         r should be(LogicalBlocks(
//           LogicalSection("div1", LogicalBlocks(
//             LogicalParagraph("content1"),
//             LogicalSection("div11", LogicalBlocks(
//               LogicalParagraph("content11"),
//               LogicalSection("div111", LogicalBlocks(
//                 LogicalParagraph("content111")
//               ))
//             )),
//             LogicalSection("div12", LogicalBlocks(
//               LogicalParagraph("content12"),
//               LogicalSection("div121", LogicalBlocks(
//                 LogicalParagraph("content121")
//               ))
//             ))
//           ))
//         ))
//       }
//    }
//  }
  // "main" should {
  //   "LogicalBlocks" in {
  //     val f = "/Users/asami/src/Project2017/EverforthFramework/src/main/resources/com/everforth/everforth/spec/doc/AppResource.org"
  //     LogicalBlocks.main(Array(f))
  //   }
  // }
}
