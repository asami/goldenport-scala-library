package org.goldenport.parser

import org.goldenport.RAISE
import org.goldenport.collection.NonEmptyVector

/*
 * @since   Aug. 24, 2019
 * @version Oct. 14, 2019
 * @author  ASAMI, Tomoharu
 */
case class CommandParser[T](
  candidates: List[CommandParser.Slot[T]],
  strategy: CommandParser.Strategy = CommandParser.NoConflictStrategy
) extends Parser {
  import CommandParser._

  def get(name: String): Option[T] = apply(name).toOption

  def apply(name: String): Result[T] = strategy match {
    case NoConflictStrategy => _apply_no_confilict(name)
    case FirstMatchStrategy => _apply_first_match(name)
  }

  private def _apply_no_confilict(name: String): Result[T] =
    candidates.find(_.name == name).map(x => Found(x.command)).getOrElse (
      candidates.filter(_.name.startsWith(name)) match {
        case Nil => NotFound()
        case x :: Nil => Found(x.command)
        case x :: xs => Candidates(NonEmptyVector(x, xs))
      }
    )

  private def _apply_first_match(name: String): Result[T] =
    candidates.find(_.name == name).map(x => Found(x.command)).getOrElse (
      candidates.find(_.name.startsWith(name)) match {
        case None => NotFound()
        case Some(x) => Found(x.command)
      }
    )

  def append(p: CommandParser[T]): CommandParser[T] = copy(candidates ::: p.candidates)
}

object CommandParser {
  private val _empty = CommandParser(Nil)
  def empty[T] = _empty.asInstanceOf[CommandParser[T]]

  sealed trait Strategy
  case object NoConflictStrategy extends Strategy
  case object FirstMatchStrategy extends Strategy

  sealed trait Result[T] {
    def toOption: Option[T]
  }
  case class NotFound[T]() extends Result[T] {
    def toOption: Option[T] = None
  }
  case class Found[T](command: T) extends Result[T] {
    def toOption: Option[T] = Some(command)
  }
  case class Candidates[T](commands: NonEmptyVector[Slot[T]]) extends Result[T] {
    def toOption: Option[T] = None
  }
  object Candidates {
    def create[T](p: (String, T), ps: (String, T)*): Candidates[T] = {
      val xs = NonEmptyVector(Slot(p._1, p._2), ps.map(kv => Slot(kv._1, kv._2)))
      Candidates(xs)
    }
  }

  case class Slot[T](name: String, command: T)

  def create[T](p: (String, T), ps: (String, T)*): CommandParser[T] = create(p +: ps)

  def create(p: String, ps: String*): CommandParser[String] = create((p, p) +: ps.map(x => (x, x)))

  def create[T](ps: Seq[(String, T)]): CommandParser[T] = {
    val xs = ps.map {
      case (k, v) => Slot(k, v)
    }
    CommandParser(xs.toList)
  }
}
