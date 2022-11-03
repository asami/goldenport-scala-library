package org.goldenport.event.logic

import scala.concurrent.duration._
import org.goldenport.event._

/*
 * @since   Oct. 26, 2022
 * @version Oct. 28, 2022
 * @author  ASAMI, Tomoharu
 */
trait ExactlyOnceJournal {
  import ExactlyOnceJournal._

  def rule: Rule

  private var _events: Vector[Event] = Vector.empty

  def to_Key(p: Event): Option[Key]

  def isAlreadyIssued(p: Event): Boolean =
    to_Key(p).fold(false)(key => _events.exists(x =>
      to_Key(x).fold(false)(_.isMatch(rule, key))))

  def add(p: Event): Unit =
    to_Key(p).foreach { key =>
      val a = _thin_out(key.timestamp, _events)
      _events = a :+ p
    }

  private def _thin_out(pts: Long, ps: Vector[Event]) = {
    val ts = pts - rule.availableDuration.toMillis
    def _predicate_(p: Event) = to_Key(p).fold(false)(_.timestamp >= ts)
    ps.filter(_predicate_)
  }
}

object ExactlyOnceJournal {
  case class Rule(
    availableDuration: FiniteDuration = 3.hours,
    defaultMargin: FiniteDuration = 3.minutes
  )
  object Rule {
    val empty = Rule()
  }

  trait Key {
    def timestamp: Long
    def margin: Option[FiniteDuration]

    def isMatch(rule: Rule, p: Key): Boolean = {
      val margin = p.margin getOrElse rule.defaultMargin
      val t = timestamp + margin.toMillis
      if (p.timestamp <= t)
        is_Match(p)
      else
        false
    }

    protected def is_Match(p: Key): Boolean
  }
}
