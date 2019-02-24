package org.goldenport.cli

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import org.goldenport.RAISE
import org.goldenport.parser._
import org.goldenport.parser.LogicalLines
import org.goldenport.parser.LogicalLines.{InitState, LogicalLinesParseState}

/*
 * @since   Oct.  4, 2018
 *  version Oct. 10, 2018
 * @version Feb. 14, 2019
 * @author  ASAMI, Tomoharu
 */
class ReadEvalPrintLoop(
  environment: Environment,
  init: ReadEvalPrintLoop.IState,
  logicalLinesConfig: LogicalLines.Config = LogicalLines.Config.default
) {
  import ReadEvalPrintLoop._

  def stdInParseEvents: Process[Task, ParseEvent] =
    Process.repeatEval(Task.delay {
      Option(System.in.read()).
        map(CharEvent.apply).
        getOrElse(EndEvent)
    })

  def execute() = {
    (Process.emit(StartEvent) fby stdInParseEvents).
      pipe(_logical_line(InitState)).
      pipe(_fsm(init)).
      through(io.stdOut).run.run
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

  private def _fsm(state: IState): Process1[ReplEvent, String] =
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
    def apply(p: ReplEvent): (IState, String)
  }

  case object SimpleState extends IState {
    val prompt = "readevalprintloop> "
    def apply(p: ReplEvent) = p match {
      case ReplStart => (this, s"$prompt")
      case ReplEnd => ???
      case ReplLine(l) =>
        val s = l.text
        (this, s"$s\n$prompt")
    }
  }

  def main(args: Array[String]) {
    val env = Environment.create(args)
    val repl = new ReadEvalPrintLoop(env, SimpleState)
    repl.execute()
  }
}
