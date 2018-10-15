package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 *  version Aug. 29, 2018
 * @version Sep. 22, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseReaderWriterState[C <: ParseConfig, AST] {
  def apply(config: C, event: ParseEvent): (ParseMessageSequence, ParseResult[AST], ParseReaderWriterState[C, AST])
}

case class ParseReaderWriterStateClass[C <: ParseConfig, AST](
  config: C,
  init: ParseReaderWriterState[C, AST]
) {
  type S = ParseReaderWriterState[C, AST]
  type B = ParseResult[AST]
  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: ParseEvent): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  def apply(events: Seq[Char]): OUT = {
    val es = CharEvent.make(events)
    // println(s"es: $es")
    _parse_events(es :+ EndEvent)
  }

  def apply(events: LogicalLines): OUT = {
    val es = LogicalLineEvent.make(events)
    // println(s"es: $es")
    _parse_events(es :+ EndEvent)
  }

  private def _parse_events(events: Seq[ParseEvent]): OUT = {
    val a = events./:(initrws)((z, x) =>
      for {
        _ <- z
        r <- action(x)
      } yield r
    )
    a.run(config, init)
  }
}
