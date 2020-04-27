package org.goldenport.parser

import scalaz._, Scalaz._  
import org.goldenport.RAISE
import org.goldenport.util.VectorUtils
  
/*
 * @since   Aug. 21, 2018
 *  version Sep. 22, 2018
 *  version Oct.  9, 2018
 *  version Dec. 31, 2018
 *  version Apr. 21, 2019
 *  version Sep. 22, 2019
 * @version Jan. 31, 2020
 * @author  ASAMI, Tomoharu
 */
trait ParseEvent {
  def location: ParseLocation
  def getLogicalLineText: Option[String] = None
}
object ParseEvent {
  import scalaz.concurrent.Task
  import scalaz.stream._

  case class SlidingConsoleState(
    input: Vector[CharEvent]
  ) {
    private def _push(p: CharEvent) = SlidingConsoleState(input :+ p)

    def in(p: CharEvent): (SlidingConsoleState, Vector[CharEvent]) = {
      p.c match {
        case '\n' => _push(p).flush
        case '\r' => _push(p).flush
        case _ => (_push(p), Vector.empty)
      }
    }

    def flush: (SlidingConsoleState, Vector[CharEvent]) = {
      val a = input.sliding(3).toVector
      val init = a.init.flatMap(_.toList match {
        case Nil => RAISE.noReachDefect
        case x :: Nil => RAISE.noReachDefect
        case x :: y :: Nil => RAISE.noReachDefect
        case x :: y :: z :: _ => Vector(x.withNexts(y, z))
      })
      val last = a.last.toList match {
        case Nil => RAISE.noReachDefect
        case x :: Nil => Vector(x)
        case x :: y :: Nil => Vector(x.withNext(y), y)
        case x :: y :: z :: _ => Vector(x.withNexts(y, z), y.withNext(z), z)
      }
      (SlidingConsoleState.init, init ++ last)
    }
  }
  object SlidingConsoleState {
    val init = SlidingConsoleState(Vector.empty)
  }

  // case class SlidingInteractiveState(
  //   next: Option[CharEvent] = None,
  //   next2: Option[CharEvent] = None
  // ) {
  //   private def _push(p: CharEvent) = SlidingState(Some(p), next)

  //   def in(p: CharEvent): (SlidingState, Vector[CharEvent]) = (next, next2) match {
  //     case (Some(n), Some(n2)) => (_push(p), Vector(p.withNexts(n, n2)))
  //     case (Some(n), None) => (_push(p), Vector(p.withNext(n)))
  //     case (None, None) => (_push(p), Vector(p))
  //     case _ => (_push(p), Vector(p))
  //   }

  //   def flush: Vector[CharEvent] = Vector.empty

  //   def flush2: Vector[CharEvent] = (next, next2) match {
  //     case (Some(n), Some(n2)) => Vector(n.withNext(n2), n2)
  //     case (Some(n), None) => Vector(n)
  //     case (None, None) => Vector.empty
  //     case _ => Vector.empty
  //   }
  // }

  def stdInParseEvents: Process[Task, ParseEvent] =
    Process.repeatEval(Task.delay {
      Option(System.in.read()).
        map(CharEvent.apply).
        getOrElse(EndEvent)
    })

  def slidingConsole: Process1[ParseEvent, ParseEvent] = slidingConsole(SlidingConsoleState.init)

  def slidingConsole(state: SlidingConsoleState): Process1[ParseEvent, ParseEvent] = Process.receive1 { evt: ParseEvent =>
    evt match {
      case StartEvent => Process.emit(StartEvent) fby slidingConsole(state)
      case EndEvent =>
        val (s, x) = state.flush
        Process.emitAll(x :+ EndEvent) fby slidingConsole(s)
      case m: CharEvent =>
        val (s, x) = state.in(m)
        Process.emitAll(x) fby slidingConsole(s)
    }
  }


  // def slidingInteractive: Process1[ParseEvent, ParseEvent] = slidingInteractive(SlidingInteractiveState())

  // def slidingInteractive(state: SlidingState): Process1[ParseEvent, ParseEvent] = Process.receive1 { evt: ParseEvent =>
  //   evt match {
  //     case StartEvent => Process.emit(StartEvent) fby slidingInteractive(state)
  //     case EndEvent => Process.emitAll(state.flush :+ EndEvent)
  //     case m: CharEvent =>
  //       val (s, x) = state.in(m)
  //       Process.emitAll(x) fby slidingInteractive(s)
  //   }
  // }
}

case class CharEvent(
  c: Char,
  next: Option[Char],
  next2: Option[Char],
  next3: Option[Char],
  location: ParseLocation
) extends ParseEvent {
  def isMatch(p: String): Boolean = p.length match {
    case 0 => false
    case 1 => p(0) == c
    case 2 => p(0) == c && Some(p(1)) == next
    case 3 => p(0) == c && Some(p(1)) == next && Some(p(2)) == next2
    case 4 => p(0) == c && Some(p(1)) == next && Some(p(2)) == next2 && Some(p(3)) == next3
    case _ => RAISE.noReachDefect
  }

  def withNext(n: CharEvent) = copy(
    next = Some(n.c)
  )

  def withNexts(n: CharEvent, n2: CharEvent) = copy(
    next = Some(n.c),
    next2 = Some(n2.c)
  )

  def withNexts(n: CharEvent, n2: CharEvent, n3: CharEvent) = copy(
    next = Some(n.c),
    next2 = Some(n2.c),
    next3 = Some(n3.c)
  )
}
object CharEvent {
  def apply(c: Char): CharEvent = CharEvent(c, None, None, None, ParseLocation.empty)

  def apply(c: Int): CharEvent = CharEvent(c.toChar, None, None, None, ParseLocation.empty)

  def make(p: String): Vector[CharEvent] = make(p.toVector)

  def make(ps: Seq[Char]): Vector[CharEvent] = {
    case class Z(
      r: Vector[CharEvent] = Vector.empty,
      line: Int = 1,
      offset: Int = 1,
      aftercr: Boolean = false
    ) {
      def +(rhs: Seq[Char]) = {
        // println(s"+:$rhs")
        val c = rhs(0)
        val nextc = rhs.lift(1)
        val next2c = rhs.lift(2)
        val next3c = rhs.lift(3)
        if (aftercr) {
          if (c == '\n')
            _next_line(c, nextc, next2c, next3c)
          else if (c == '\r')
            _next_line(c, nextc, next2c, next3c)
          else
            _add_after_cr(c, nextc, next2c, next3c)
        } else {
          if (c == '\n')
            _next_line(c, nextc, next2c, next3c)
          else if (c == '\r')
            _same_line(c, nextc, next2c, next3c)
          else
            _same_line(c, nextc, next2c, next3c)
        }
      }

      private def _same_line(c: Char, nextc: Option[Char], next2c: Option[Char], next3c: Option[Char]) =
        _add(c, nextc, next2c, next3c, line, offset + 1)

      private def _next_line(c: Char, nextc: Option[Char], next2c: Option[Char], next3c: Option[Char]) =
        _add(c, nextc, next2c, next3c, line + 1, 1)

      private def _add(c: Char, nextc: Option[Char], next2c: Option[Char], next3c: Option[Char], l: Int, o: Int) = {
        val evt = CharEvent(c, nextc, next2c, next3c, ParseLocation(line, offset))
        Z(r = r :+ evt, l, o, c == '\r')
      }

      private def _add_after_cr(c: Char, nextc: Option[Char], next2c: Option[Char], next3c: Option[Char]) = {
        val l = line + 1
        val o = 1
        val evt = CharEvent(c, nextc, next2c, next3c, ParseLocation(line + 1, o))
        Z(r = r :+ evt, l, o + 1, c == '\r')
      }
    }
    VectorUtils.sliding4(ps)./:(Z())(_+_).r
  }

  def makeWithoutLocation(p: String): Vector[CharEvent] = makeWithoutLocation(p.toVector)

  def makeWithoutLocation(ps: Seq[Char]): Vector[CharEvent] = ps.map(apply).toVector
}

case class LogicalLineEvent(
  line: LogicalLine
) extends ParseEvent {
  def location = line.location.getOrElse(ParseLocation.empty)
  def getSectionTitle = line.getSectionTitle
  def getSectionUnderline = line.getSectionUnderline
  def isEmptyLine = line.isEmptyLine
  override def getLogicalLineText = Some(line.text)
}
object LogicalLineEvent {
  def make(p: LogicalLines): Vector[LogicalLineEvent] = p.lines.map(LogicalLineEvent.apply)
}

case object StartEvent extends ParseEvent {
  val location = ParseLocation.empty
}
case object EndEvent extends ParseEvent {
  val location = ParseLocation.empty
}

case class LineEndEvent(location: ParseLocation) extends ParseEvent
