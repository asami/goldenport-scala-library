package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Sep.  3, 2018
 * @version Sep.  3, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseReaderWriterStateBase[C <: ParseConfig, EVENT, AST] {
  def apply(config: C, event: EVENT): (ParseMessageSequence, ParseResult[AST], ParseReaderWriterStateBase[C, EVENT, AST])
}

trait ParseReaderWriterStateClassBase[C <: ParseConfig, EVENT, AST] {
  def config: C
  def init: ParseReaderWriterStateBase[C, EVENT, AST]
  type S = ParseReaderWriterStateBase[C, EVENT, AST]
  type B = ParseResult[AST]
  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: EVENT): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  // def apply(events: Seq[Char]): OUT = {
  //   val es = CharEvent.make(events)
  //   _parse_events(es :+ EndEvent)
  // }

  def parse(events: Seq[EVENT]): OUT = {
    val a = events./:(initrws)((z, x) =>
      for {
        _ <- z
        r <- action(x)
      } yield r
    )
    a.run(config, init)
  }
}
