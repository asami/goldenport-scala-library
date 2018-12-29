package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Nov. 12, 2018
 * @version Dec.  2, 2018
 * @author  ASAMI, Tomoharu
 */
trait LogicalLineReaderWriterState[C <: ParseConfig, AST] {
//  type STATE = LogicalLineReaderWriterState[C, AST]
//  type TRANSITION = (ParseMessageSequence, ParseResult[AST], LogicalLineReaderWriterState[C, AST])
  def result: AST
  def apply(config: C, event: ParseEvent): (ParseMessageSequence, ParseResult[AST], LogicalLineReaderWriterState[C, AST])

  protected def transit_next[T <: LogicalLineReaderWriterState[C, AST]](next: T): (ParseMessageSequence, ParseResult[AST], T) =
    (ParseMessageSequence.empty, ParseResult.empty, next)

  protected def transit_result_next[T <: LogicalLineReaderWriterState[C, AST]](result: AST, next: T): (ParseMessageSequence, ParseResult[AST], T) =
    (ParseMessageSequence.empty, ParseSuccess(result), next)

  protected def transit_none[T <: LogicalLineReaderWriterState[C, AST]]: (ParseMessageSequence, ParseResult[AST], T) =
    (ParseMessageSequence.empty, ParseResult.empty, this.asInstanceOf[T])
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

  def action(event: ParseEvent): RWS = ReaderWriterState((c, s) => s.apply(c, event))

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
    val evts: Seq[ParseEvent] = lines.map(LogicalLineEvent.apply)
    _parse_events(StartEvent +: evts :+ EndEvent)
  }

  private def _parse_events(lines: Seq[ParseEvent]): OUT = {
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
