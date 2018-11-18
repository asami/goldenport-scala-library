package org.goldenport.parser

import scalaz._, Scalaz._  
import scalaz.concurrent.Task
import scalaz.stream._
  
/*
 * @since   Aug. 21, 2018
 *  version Aug. 29, 2018
 *  version Sep. 22, 2018
 *  version Oct. 10, 2018
 * @version Nov. 12, 2018
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
  type B = ParseResult[Vector[AST]]
//  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

//  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

//  def action(event: ParseEvent): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  def apply(events: Seq[Char]): OUT = {
    val es = CharEvent.make(events)
    _parse_events(es)
  }

  def apply(events: LogicalLines): OUT = {
    val es = LogicalLineEvent.make(events)
    _parse_events(es)
  }

  private def _parse_events(events: Seq[ParseEvent]): OUT = {
    val x = Process.emitAll(StartEvent +: events :+ EndEvent).toSource.
      pipe(_fsm(init))
    val a = x.runLog.run
    val warns = ParseMessageSequence.empty // TODO
    val r = a.toVector
    val newstate = init // TODO
    (warns, ParseSuccess(r), newstate)
  }

  private def _fsm(state: S): Process1[ParseEvent, AST] =
    Process.receive1 { evt: ParseEvent =>
      val (msg, r, newstate) = state.apply(config, evt)
      r match {
        case m: EmptyParseResult[AST] => _fsm(newstate)
        case ParseSuccess(x, warns) => Process.emit(x) fby _fsm(newstate)
        case ParseFailure(errs, warns) => ???
      }
    }

  // def apply(events: Seq[Char]): OUT = {
  //   val es = CharEvent.make(events)
  //   // println(s"es: $es")
  //   _parse_events(es :+ EndEvent)
  // }

  // def apply(events: LogicalLines): OUT = {
  //   val es = LogicalLineEvent.make(events)
  //   // println(s"es: $es")
  //   _parse_events(es :+ EndEvent)
  // }

  // private def _parse_events(events: Seq[ParseEvent]): OUT = {
  //   val a = events./:(initrws)((z, x) =>
  //     for {
  //       _ <- z
  //       r <- action(x)
  //     } yield z + r
  //   )
  //   a.run(config, init)
  // }
}
