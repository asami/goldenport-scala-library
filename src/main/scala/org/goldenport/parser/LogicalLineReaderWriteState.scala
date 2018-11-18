package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Nov. 12, 2018
 * @version Nov. 12, 2018
 * @author  ASAMI, Tomoharu
 */
trait LogicalLineReaderWriterState[C <: ParseConfig, AST] {
  def result: AST
  def apply(config: C, event: LogicalLine): (ParseMessageSequence, ParseResult[AST], LogicalLineReaderWriterState[C, AST])
}

case class LogicalLineReaderWriterStateClass[C <: ParseConfig, AST](
  config: C,
  init: LogicalLineReaderWriterState[C, AST]
) {
  type S = LogicalLineReaderWriterState[C, AST]
  type B = ParseResult[AST]
  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: LogicalLine): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  def apply(s: String): OUT = {
    val lines = LogicalLines.parse(s)
    apply(lines)
  }

  def apply(p: LogicalLines): OUT = apply(p.lines)

  def apply(ps: Seq[LogicalLine]): OUT = {
    val r = _parse_lines(ps)
    println(s"LogicalLineReaderWriterStateClass: ${ps} => $r")
    r
  }

  private def _parse_lines(lines: Seq[LogicalLine]): OUT = {
    val a = lines./:(initrws)((z, x) =>
      for {
        _ <- z
        r <- action(x)
      } yield {
        println(s"LogicalLineReaderWriterStateClass#_parse_lines: $r")
        r
      }
    )
    a.run(config, init)
  }
}
