package org.goldenport.values

import scalaz._, Scalaz._
import org.joda.time.DateTimeZone
import org.goldenport.Strings
import org.goldenport.parser._

/*
 * @since   Feb. 14, 2021
 * @version May.  2, 2021
 * @author  ASAMI, Tomoharu
 */
case class TriggerSchedule(
  slots: Vector[TriggerSchedule.Slot] = Vector.empty
) {
  def +(rhs: TriggerSchedule): TriggerSchedule = TriggerSchedule(slots ++ rhs.slots)
}

object TriggerSchedule {
  val empty = TriggerSchedule()

  implicit def TriggerScheduleMonoid = new Monoid[TriggerSchedule] {
    def zero = empty
    def append(lhs: TriggerSchedule, rhs: => TriggerSchedule) = lhs + rhs
  }

  sealed trait Action {
  }
  case class LabelAction(label: String) extends Action {
  }

  case class Slot(guard: TriggerGuard, action: Action)

  def parse(tz: DateTimeZone, p: String): ParseResult[TriggerSchedule] = {
    val lines = Strings.tolines(p)
    for {
      xs <- lines.traverse(_parse_line(tz, _))
    } yield TriggerSchedule(xs)
  }

  private def _parse_line(tz: DateTimeZone, p: String): ParseResult[Slot] = {
    val values = Strings.totokens(p, " ")
    for {
      guard <- CronTriggerGuard.parse(tz, values)
      action <- {
        val xs = values.drop(5)
        val action = xs.lift(0).map(LabelAction)
        ParseResult.successOrError("Missing action", action)
      }
    } yield Slot(guard, action)
  }
}
