package org.goldenport.event

import scalaz._, Scalaz._
import org.goldenport.parser.ParseResult
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.collection.TreeMap

/*
 * @since   May.  5, 2021
 * @version May. 23, 2021
 * @author  ASAMI, Tomoharu
 */
case class EventRule(
  classes: TreeMap[EventClazz] = TreeMap.empty
) {
  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[EventRule] = if (isEmpty) None else Some(this)

  def +(rhs: EventRule): EventRule = copy(classes = classes + rhs.classes)

  def createOption(name: String): Option[Event] = classes.get(name).map(_.create())
}

object EventRule {
  val PROP_EVT_EVENT = "event"
  val PROP_EVT_NAME = "name"

  implicit object EventRuleMonoid extends Monoid[EventRule] {
    def zero = EventRule.empty
    def append(lhs: EventRule, rhs: => EventRule) = lhs + rhs
  }

  val empty = EventRule()

  case class Config()
  object Config {
    val default = Config()
  }

  def apply(p: EventClazz): EventRule = ???

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

    private def _events(p: Hocon): ParseResult[List[EventClazz]] = {
      val cs = p.takeConfigList(PROP_EVT_EVENT)
      cs.traverse(_event)
    }

    private def _event(p: Hocon): ParseResult[EventClazz] =
      for {
        name <- p.parseString(PROP_EVT_NAME)
      } yield EventClazz(name)
  }
}
