package org.goldenport.parser

import scalaz._, Scalaz._  
import scalaz.concurrent.Task
import scalaz.stream._
import org.goldenport.RAISE
  
/*
 * @since   Aug. 21, 2018
 *  version Aug. 29, 2018
 *  version Sep. 22, 2018
 *  version Oct. 10, 2018
 *  version Nov. 12, 2018
 *  version Jan.  5, 2019
 *  version Feb. 16, 2019
 *  version May.  6, 2019
 *  version Sep. 22, 2019
 *  version Nov. 29, 2020
 * @version Jan. 16, 2021
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
    val es = if (config.isLocation)
      CharEvent.make(events)
    else
      CharEvent.makeWithoutLocation(events)
    // println(s"ParseReaderWriterState#apply $es")
    _parse_events(es)
  }

  def apply(events: LogicalLines): OUT = {
    val es = LogicalLineEvent.make(events)
    _parse_events(es)
  }

  private def _parse_events(events: Seq[ParseEvent]): OUT = {
    val es = StartEvent +: events :+ EndEvent
    val x = Process.emitAll(es).toSource.
      pipe(_fsm(init))
    val a = x.runLog.run
    // println(s"ParseReaderWriterState#_parse_events: $a")
    val warns = ParseMessageSequence.empty // TODO
    val r = a.toVector
    val newstate = init // TODO
    (warns, ParseSuccess(r), newstate)
  }

  private def _fsm(state: S): Process1[ParseEvent, AST] =
    Process.receive1 { evt: ParseEvent =>
      val (msg, r, newstate) = state.apply(config, evt)
      // println(s"ParseReaderWriterState#_fsm[$evt]: $r")
      r match {
        case m: EmptyParseResult[AST] => _fsm(newstate)
        case ParseSuccess(x, warns) =>
          if (x == null)
            RAISE.noReachDefect(s"ParseReaderWriterState#_fsm[$evt]: $r")
          else
            Process.emit(x) fby _fsm(newstate) // TODO warns
        case ParseFailure(es, ws) =>
          val errs = es ++ msg.errors
          val warns = ws ++ msg.warnings
          throw ParseSyntaxErrorException(errs, warns)
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
