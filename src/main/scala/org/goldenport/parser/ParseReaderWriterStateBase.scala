package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Sep.  3, 2018
 * @version Nov. 18, 2018
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
  type OUT = (ParseMessageSequence, Vector[B], S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: EVENT): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  // def apply(events: Seq[Char]): OUT = {
  //   val es = CharEvent.make(events)
  //   _parse_events(es :+ EndEvent)
  // }

  def parse(events: Seq[EVENT]): OUT = {
    case class Z(
      msgs: ParseMessageSequence = ParseMessageSequence.empty,
      rs: Vector[B] = Vector.empty,
      state: S = init
    ) {
      def r: OUT = (msgs, rs, state)
      def +(rhs: EVENT): Z = {
        val (ms, r, s) = state.apply(config, rhs)
        r match {
          case _: EmptyParseResult[AST] => Z(msgs + ms, rs, s)
          case m: ParseSuccess[AST] => Z(msgs + ms :++ m.warnings, rs :+ m, s)
          case m: ParseFailure[AST] => Z(msgs + ms :++ m.warnings :++ m.errors, rs, s)
        }
      }
    }
    events./:(Z())(_+_).r
    // val a = events./:(initrws)((z, x) =>
    //   for {
    //     _ <- z
    //     r <- action(x)
    //   } yield r
    // )
    // a.run(config, init)
  }
}
