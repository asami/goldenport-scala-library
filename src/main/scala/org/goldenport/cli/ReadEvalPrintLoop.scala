package org.goldenport.cli

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import java.io.InputStreamReader
import org.goldenport.RAISE
import org.goldenport.parser._
import org.goldenport.parser.LogicalLines
import org.goldenport.parser.LogicalLines.{InitState, LogicalLinesParseState}
import org.goldenport.console._

/*
 * @since   Oct.  4, 2018
 *  version Feb. 14, 2019
 *  version Apr. 21, 2019
 *  version Jan. 20, 2020
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
class ReadEvalPrintLoop(
  console: ConsoleManager,
  init: ReadEvalPrintLoop.IState,
  logicalLinesConfig: LogicalLines.Config = LogicalLines.Config.default
) {
  import ReadEvalPrintLoop._

//  private val _system_in_reader = new InputStreamReader(System.in, environment.consoleCharset)

  // TODO migrate to ParseEvent
  def stdInParseEvents: Process[Task, ParseEvent] =
    Process.repeatEval(Task.delay {
      Option(console.read()).
        map(CharEvent.apply).
        getOrElse(EndEvent)
    })

  def execute() = {
    (Process.emit(StartEvent) fby stdInParseEvents).
      pipe(ParseEvent.slidingConsole).
      pipe(_logical_line(InitState)).
      pipe(_fsm(init)).
      through(_sink).run.run
  }

  private def _sink: Sink[Task, MessageSequence] = {
    scalaz.stream.io.channel((o: MessageSequence) => Task.delay {
      console.message(o)
    })
  }

  private def _logical_line(state: LogicalLinesParseState): Process1[ParseEvent, ReplEvent] =
    Process.receive1 { evt: ParseEvent =>
      // println(s"_logical_line: $evt")
      def go: Process1[ParseEvent, ReplEvent] = {
        val (msgs, result, newstate) = state(logicalLinesConfig, evt)
        // println(s"$result")
        // println(s"$newstate")
        result match {
          case m: EmptyParseResult[LogicalLines] => _logical_line(newstate)
          case ParseSuccess(x, warns) =>
            val evts = x.lines.map(ReplLine)
            Process.emitAll(evts) fby _logical_line(newstate)
          case ParseFailure(errs, warns) => RAISE.notImplementedYetDefect
        }
      }
      evt match {
        case StartEvent => Process.emit(ReplStart) fby go
        case _ => go
      }
    }

  private def _fsm(state: IState): Process1[ReplEvent, MessageSequence] =
    Process.receive1 { evt: ReplEvent =>
      val (newstate, output) = state.apply(evt)
      Process.emit(output) fby _fsm(newstate)
    }
}

object ReadEvalPrintLoop {
  sealed trait ReplEvent
  case object ReplStart extends ReplEvent
  case object ReplEnd extends ReplEvent
  case class ReplLine(line: LogicalLine) extends ReplEvent

  trait IState {
    def apply(p: ReplEvent): (IState, MessageSequence)
  }

  case object SimpleState extends IState {
    val prompt = "readevalprintloop> "
    def apply(p: ReplEvent) = p match {
      case ReplStart => (this, MessageSequence(s"$prompt"))
      case ReplEnd => RAISE.notImplementedYetDefect
      case ReplLine(l) =>
        val s = l.text
        (this, MessageSequence(s"$s\n$prompt"))
    }
  }

  def main(args: Array[String]) {
    val env = Environment.create(args)
    val config = ConsoleManager.Config(env.charset, env.newline)
    val console = ConsoleManager.create(env)
    val repl = new ReadEvalPrintLoop(console, SimpleState)
    repl.execute()
  }
}
