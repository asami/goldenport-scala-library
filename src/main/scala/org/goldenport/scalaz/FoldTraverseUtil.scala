package org.goldenport.scalaz

import scala.language.higherKinds
import scalaz._, Scalaz._
import org.goldenport.context.Consequence

/*
 * @since   Oct.  1, 2025
 * @version Oct. 17, 2025
 * @author  ASAMI, Tomoharu
 */
object FoldTraverseUtil {
  def intercalateTraverse[F[_]: Monad, A, B](
    as: Seq[A],
    sep: F[B]
  )(f: A => F[B]): F[Vector[B]] = {
    as match {
      case Nil => Monad[F].point(Vector.empty[B])
      case Seq(a) => f(a).map(Vector(_))
      case _ =>
        def loop(xs: Seq[A]): F[Vector[B]] = xs match {
          case Nil => Monad[F].point(Vector.empty)
          case Seq(x) => f(x).map(Vector(_))
          case x +: rest =>
            for {
              h <- f(x)
              s <- sep
              t <- loop(rest)
            } yield h +: s +: t
        }
        loop(as)
    }
  }

  def intercalateTraverse_[F[_]: Monad, A](
    as: Seq[A],
    sep: F[Unit]
  )(f: A => F[Unit]): F[Unit] =
    intercalateTraverse[F, A, Unit](as, sep)(f).void

  def intercalateTraverseWithEnd[F[_]: Monad, A, B](
    as: Seq[A],
    sep: F[B],
    end: F[B]
  )(f: A => F[B]): F[Vector[B]] = as match {
    case Nil => Vector.empty[B].point[F]
    case Seq(a) =>
      for {
        x <- f(a)
        e <- end
      } yield Vector(x, e)
    case x +: rest =>
      def loop(xs: Seq[A]): F[Vector[B]] = xs match {
        case Nil => end.map(Vector(_))
        case Seq(a) =>
          for {
            x <- f(a)
            e <- end
          } yield Vector(x, e)
        case a +: tail =>
          for {
            x <- f(a)
            s <- sep
            t <- loop(tail)
          } yield x +: s +: t
      }
      loop(as)
  }

  def intercalateTraverseWithEnd_[F[_]: Monad, A](
    as: Seq[A],
    sep: F[Unit],
    end: F[Unit]
  )(f: A => F[Unit]): F[Unit] = as match {
    case Nil => ().point[F]
    case Seq(a) =>
      for {
        _ <- f(a)
        _ <- end
      } yield ()
    case x +: rest =>
      def loop(xs: Seq[A]): F[Unit] = xs match {
        case Nil => end
        case Seq(a) =>
          for {
            _ <- f(a)
            _ <- end
          } yield ()
        case a +: tail =>
          for {
            _ <- f(a)
            _ <- sep
            _ <- loop(tail)
          } yield ()
      }
      loop(as)
  }

  def intercalateFoldLeft[B: Monoid](
    as: Seq[B],
    sep: B
  ): B = as match {
    case Nil => Monoid[B].zero
    case Seq(a) => a
    case a +: rest => rest.foldLeft(a)((acc, b) => acc |+| sep |+| b)
  }

  def intercalateFoldMap[A, B: Monoid](
    as: Seq[A],
    sep: B
  )(f: A => B): B = as match {
    case Nil => Monoid[B].zero
    case Seq(a) => f(a)
    case a +: rest => rest.foldLeft(f(a))((acc, x) => acc |+| sep |+| f(x))
  }

  def intercalateFoldMap_[A, B: Monoid](
    as: Seq[A],
    sep: B
  )(f: A => B): Unit = {
    val _ = intercalateFoldMap(as, sep)(f)
    ()
  }
} 
