package org.goldenport.cli

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.value._
import org.goldenport.values.NumberRange
import org.goldenport.values.NumberInterval
import org.goldenport.util.NumberUtils

/*
 * @since   Jul. 23, 2023
 * @version Jul. 24, 2023
 * @author  ASAMI, Tomoharu
 */
case class ListingDirective(
  baseindex: ListingDirective.BaseIndex,
  strategy: ListingDirective.Strategy
) {
  def apply[T, S[_] <: Seq[_]](ps: S[T])(implicit bf: CanBuildFrom[S[T], T, S[T]]): S[T] = {
    val r: S[T] = strategy(this, ps)
    val a = bf(r)
    val iterator: Iterator[T] = r.iterator.asInstanceOf[Iterator[T]]
    while (iterator.hasNext)
      a += iterator.next
    a.result()
  }
}

object ListingDirective {
  sealed trait BaseIndex extends NamedValueInstance {
  }
  object BaseIndex extends EnumerationClass[BaseIndex] {
    val elements = Vector(Zero, One)

    case object Zero extends BaseIndex {
      val name = "zero"
    }
    case object One extends BaseIndex {
      val name = "one"
    }
  }

  sealed trait Strategy {
    def apply[T, S[_] <: Seq[_]](ctx: ListingDirective, ps: S[T]): S[T] =
      apply_seq(ctx, ps.asInstanceOf[Seq[T]]).asInstanceOf[S[T]]

    protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T]
  }
  object Strategy {
    case object Whole extends Strategy {
      protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T] = ps
    }
    case class Init(size: Int) extends Strategy {
      protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T] = ps.take(size)
    }
    case class Tail(size: Int) extends Strategy {
      protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T] = ps.takeRight(size)
    }
    case class Range(range: NumberRange) extends Strategy {
      protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T] = {
        val a = ps.zipWithIndex
        val b = ctx.baseindex match {
          case BaseIndex.Zero => a
          case BaseIndex.One => a map {
            case (x, i) => (x, i + 1)
          }
        }
        b.flatMap {
          case (x, i) => if (range.isValid(i)) Some(x) else None
        }
      }
    }
    case class Interval(interval: NumberInterval) extends Strategy {
      protected def apply_seq[T](ctx: ListingDirective, ps: Seq[T]): Seq[T] = RAISE.notImplementedYetDefect
    }
  }

  case class Builder(
    baseindex: BaseIndex = BaseIndex.Zero,
    position: Builder.Position = Builder.Position.Init
  ) {
    def apply(p: Argument): Consequence[ListingDirective] = apply(p.asString)

    def apply(p: String): Consequence[ListingDirective] =
      if (p.contains('~') || p.contains(','))
        for {
          r <- Consequence.from(NumberRange.parse(p))
        } yield ListingDirective(baseindex, Strategy.Range(r))
      else
        NumberUtils.getInt(p) match {
          case Some(s) =>
            val strategy = position match {
              case Builder.Position.Init => Strategy.Init(s)
              case Builder.Position.Tail => Strategy.Tail(s)
            }
            Consequence.success(ListingDirective(baseindex, strategy))
          case None => p match {
            case "whole" => Consequence.success(ListingDirective(baseindex, Strategy.Whole))
            case m => Consequence.invalidArgumentFault("range", m)
          }
        }
  }
  object Builder {
    sealed trait Position extends NamedValueInstance {
    }
    object Position extends EnumerationClass[Position] {
      val elements = Vector(Init, Tail)

      case object Init extends Position {
        val name = "init"
      }
      case object Tail extends Position {
        val name = "tail"
      }
    }

    def oneInit: Builder = Builder(BaseIndex.One, Builder.Position.Init)
    def oneTail: Builder = Builder(BaseIndex.One, Builder.Position.Tail)
  }

  def parse(p: Argument): Consequence[ListingDirective] = Builder().apply(p)

  def parseInit(p: Argument): Consequence[ListingDirective] = Builder().apply(p)

  def parseTail(p: String): Consequence[ListingDirective] = Builder(position = Builder.Position.Tail).apply(p)

  def parseOne(p: Argument): Consequence[ListingDirective] =
    Builder(BaseIndex.One).apply(p)

  def parseOneTail(p: Argument): Consequence[ListingDirective] =
    Builder(BaseIndex.One, Builder.Position.Tail).apply(p)

  def init(size: Int): ListingDirective = ListingDirective(BaseIndex.Zero, Strategy.Init(size))

  def tail(size: Int): ListingDirective = ListingDirective(BaseIndex.Zero, Strategy.Tail(size))

  def oneTail(size: Int): ListingDirective = ListingDirective(BaseIndex.One, Strategy.Tail(size))
}
