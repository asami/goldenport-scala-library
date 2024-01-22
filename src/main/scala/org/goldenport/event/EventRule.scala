package org.goldenport.event

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.RAISE
import org.goldenport.context.Showable
import org.goldenport.parser.ParseResult
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.collection.TreeMap

/*
 * @since   May.  5, 2021
 *  version May. 23, 2021
 *  version Jun. 13, 2021
 * @version Aug.  6, 2023
 * @author  ASAMI, Tomoharu
 */
case class EventRule(
  classes: TreeMap[EventClazz] = TreeMap.empty
) extends Showable {
  def print: String = "???"

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[EventRule] = if (isEmpty) None else Some(this)

  def +(rhs: EventRule): EventRule = copy(classes = classes + rhs.classes)

  def createOption(name: String): Option[Event] = classes.get(name).map(_.create())

  def createCallOption(name: String, id: ObjectId): Option[Event] = classes.get(name).map(_.createCall(id))
}

object EventRule {
  implicit object EventRuleMonoid extends Monoid[EventRule] {
    def zero = EventRule.empty
    def append(lhs: EventRule, rhs: => EventRule) = lhs + rhs
  }

  val empty = EventRule()

  case class Config()
  object Config {
    val default = Config()
  }

  def apply(p: EventClazz): EventRule = RAISE.notImplementedYetDefect

  def parse(p: String): ParseResult[EventRule] = Builder().build(p)

  def build(p: Hocon): ParseResult[EventRule] = Builder().build(p)

  case class Builder(
    config: Config = Config.default
  ) {
    def build(rule: String): ParseResult[EventRule] = {
      val hocon = ConfigFactory.parseString(rule)
      build(hocon)
    }

    def build(hocon: Hocon): ParseResult[EventRule] = for {
      ev <- _events(hocon)
    } yield EventRule(TreeMap.create(".", ev.map(x => x.name -> x)))

    private def _events(p: Hocon): ParseResult[List[EventClazz]] = for {
      a <- p.parseConfigOrConfigList(PROP_EVT_EVENT)
      b <- a match {
        case Right(r) => r.traverse(_event)
        case Left(l) => _event_class_list(l)
      }
    } yield b

    private def _event(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)

    private def _event_class_list(p: Hocon): ParseResult[List[EventClazz]] = for {
      a0 <- p.parseAsConfigList(PROP_EVT_SIGNAL)
      a <- _signal(a0)
      b0 <- p.parseAsConfigList(PROP_EVT_CALL)
      b <- _call(b0)
      c0 <- p.parseAsConfigList(PROP_EVT_CHANGE)
      c <- _change(c0)
      d0 <- p.parseAsConfigList(PROP_EVT_TIME)
      d <- _time(d0)
    } yield (a ++ b ++ c ++ d).toList

    private def _signal(ps: List[Hocon]): ParseResult[List[EventClazz]] =
      ps.traverse(_signal)

    private def _signal(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)

    private def _call(ps: List[Hocon]): ParseResult[List[EventClazz]] =
      ps.traverse(_call)

    private def _call(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)

    private def _change(ps: List[Hocon]): ParseResult[List[EventClazz]] =
      ps.traverse(_change)

    private def _change(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)

    private def _time(ps: List[Hocon]): ParseResult[List[EventClazz]] =
      ps.traverse(_time)

    private def _time(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)
  }
}
