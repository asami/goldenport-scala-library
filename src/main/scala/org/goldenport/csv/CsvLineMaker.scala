package org.goldenport.csv

import scalaz._, Scalaz._
import scalaz.stream._

/*
 * Derived from com.asamioffice.goldenport.text.CsvLineMaker
 *
 * See:
 * - http://modegramming.blogspot.jp/2015/03/scalaoop.html
 * - http://modegramming.blogspot.jp/2015/03/scalafp.html
 * - http://modegramming.blogspot.jp/2015/04/scalaz-stream.html
 * 
 * @since   Sep. 25, 2015
 *  version Mar.  3, 2016
 *  version Aug. 30, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
object CsvLineMaker {
  type CsvField = Option[String]
  type CsvFields = Vector[CsvField]
  type CsvRecord = Vector[CsvField]
  val EOT: Char = 0x04
  val EOTString = new String(Array[Char](0x04))

  private val _no_output: Vector[Vector[CsvField]] = Vector.empty

  sealed trait Event
  case object EndEvent extends Event
  case class InputEvent(c: Char) extends Event
  case class EscapedInputEvent(c: Char) extends Event

  sealed trait ParseState {
    def fields: CsvFields
    def apply(event: Event): (ParseState, Vector[CsvRecord])

    protected def make_fields(fields: CsvFields, buffer: Vector[Char]): CsvFields = {
      fields :+ Some(buffer.mkString)
    }
  }

  case object InitState extends ParseState {
    val fields = Vector.empty
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => (EndState, Vector.empty)
        case InputEvent(c) => c match {
          case ',' => (NeutralState(fields :+ None), _no_output)
          case '"' => (DoubleQuoteState(fields, Vector.empty), _no_output)
          case '\\' => (EscapeState(this), _no_output)
          case '\n' => (InitState, Vector.empty)
          case '\r' => (InitState, Vector.empty)
          case EOT => (EndState, Vector.empty)
          case x => (TextState(fields, Vector(c)), _no_output)
        }
        case EscapedInputEvent(c) => (TextState(fields, Vector(c)), _no_output)
      }
    }
  }

  case object EndState extends ParseState {
    val fields = Vector.empty
    def apply(event: Event): (ParseState, Vector[CsvRecord]) =
      (this, Vector.empty)
  }

  case class NeutralState(fields: CsvFields) extends ParseState {
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => (EndState, Vector(fields :+ None))
        case InputEvent(c) => c match {
          case ',' => (NeutralState(fields :+ None), _no_output)
          case '"' => (DoubleQuoteState(fields, Vector.empty), _no_output)
          case '\\' => (EscapeState(this), _no_output)
          case '\n' => (InitState, Vector(fields))
          case '\r' => (InitState, Vector(fields))
          case EOT => (EndState, Vector(fields :+ None))
          case x => (TextState(fields, Vector(c)), _no_output)
        }
        case EscapedInputEvent(c) => (TextState(fields, Vector(c)), _no_output)
      }
    }
  }

  case class TextState(fields: CsvFields, buffer: Vector[Char]) extends ParseState {
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => (EndState, Vector(make_fields(fields, buffer)))
        case InputEvent(c) => c match {
          case ',' => (NeutralState(make_fields(fields, buffer)), _no_output)
          case '"' => (DoubleQuoteState(fields, buffer), _no_output)
          case '\\' => (EscapeState(this), _no_output)
          case '\n' => (InitState, Vector(make_fields(fields, buffer)))
          case '\r' => (InitState, Vector(make_fields(fields, buffer)))
          case EOT => (EndState, Vector(make_fields(fields, buffer)))
          case x => (TextState(fields, buffer :+ c), _no_output)
        }
        case EscapedInputEvent(c) => (TextState(fields, buffer :+ c), _no_output)
      }
    }
  }

  case class EscapeState(parent: ParseState) extends ParseState {
    def fields = parent.fields
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => parent(event)
        case InputEvent(c) => parent(EscapedInputEvent(c))
        case EscapedInputEvent(c) => parent(EscapedInputEvent(c))
      }
    }
  }

  case class DoubleQuoteState(fields: CsvFields, buffer: Vector[Char]) extends ParseState {
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => (EndState, Vector(make_fields(fields, buffer)))
        case InputEvent(c) => c match {
          case '"' => (DoubleQuoteInDoubleQuoteState(fields, buffer), _no_output)
          case '\\' => (EscapeState(this), _no_output)
          case x => (DoubleQuoteState(fields, buffer :+ x), _no_output)
        }
        case EscapedInputEvent(c) => (DoubleQuoteState(fields, buffer :+ c), _no_output)
      }
    }
  }

  case class DoubleQuoteInDoubleQuoteState(fields: CsvFields, buffer: Vector[Char]) extends ParseState {
    def apply(event: Event): (ParseState, Vector[CsvRecord]) = {
      event match {
        case EndEvent => (EndState, Vector(make_fields(fields, buffer)))
        case InputEvent(c) => c match {
          case '"' => (DoubleQuoteState(fields, buffer :+ '"'), _no_output)
          case ',' => (NeutralState(make_fields(fields, buffer)), _no_output)
          case '\\' => (EscapeState(this), _no_output) // no reach
          case '\n' => (InitState, Vector(make_fields(fields, buffer)))
          case '\r' => (InitState, Vector(make_fields(fields, buffer)))
          case EOT => (EndState, Vector(make_fields(fields, buffer)))
          case x => (TextState(fields, buffer :+ c), _no_output) // no reach
        }
        case EscapedInputEvent(c) => (DoubleQuoteState(fields, buffer :+ c), _no_output)
      }
    }
  }

  def action(event: Char) = State((s: ParseState) => {
    s(InputEvent(event))
  })

  def fsmInit: Process1[String, CsvRecord] = fsm(InitState)

  // def fsm(state: ParseState): Process1[String, CsvRecord] =
  //   Process.receive1Or {
  //     state(EndEvent) match {
  //       case (s, xs) if xs.isEmpty => fsm(s)
  //       case (s, xs) => Process.emitAll(xs) fby fsm(s)
  //     }
  //   } { x: String =>
  //     val (s, xs) = x.toVector.traverseS(action).run(state)
  //     xs.flatten match {
  //       case m if m.isEmpty => fsm(s)
  //       case m => Process.emitAll(m) fby fsm(s)
  //     }
  //   }

  // Process.receive1Or causes a compiler bug.
  def fsm(state: ParseState): Process1[String, CsvRecord] =
    Process.receive1 { x: String =>
      val (s, xs) = x.toVector.traverseS(action).run(state)
      xs.flatten match {
        case m if m.isEmpty => fsm(s)
        case m => Process.emitAll(m) fby fsm(s)
      }
    }

  def parse(s: String): CsvRecord =
    parseWhole(s).headOption.getOrElse(Vector.empty)

  def parseWhole(s: String): Vector[CsvRecord] = {
    val a = s.toVector.traverseS(action)
    val z = a.run(InitState)
    val (b, c) = z
    val (d, e) = b(EndEvent)
    c.flatten ++ e
  }
}
