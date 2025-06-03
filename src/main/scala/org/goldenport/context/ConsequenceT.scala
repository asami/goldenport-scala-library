package org.goldenport.context

import scala.language.higherKinds
import scalaz._, Scalaz._
import scalaz.syntax.functor._
import scalaz.syntax.monad._
import Consequence.ConsequenceMonad

/*
 * @since   May. 19, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
case class ConsequenceT[F[_], A](run: F[Consequence[A]])

object ConsequenceT {
  // Smart constructor: lifts pure value into the transformer
  def point[F[_]: Applicative, A](a: A): ConsequenceT[F, A] =
    ConsequenceT(Applicative[F].point(Consequence.success(a)))

  // Smart constructor: lifts effectful value into the transformer
  def liftM[F[_]: Functor, A](fa: F[A]): ConsequenceT[F, A] = {
    val F = implicitly[Functor[F]]
    ConsequenceT(F.map(fa)(Consequence.success(_)))
  }

  // Monad instance for ConsequenceT
  implicit def monadInstance[F[_]](implicit F: Monad[F]): Monad[({ type λ[α] = ConsequenceT[F, α] })#λ] =
    new Monad[({ type λ[α] = ConsequenceT[F, α] })#λ] {
      def point[A](a: => A): ConsequenceT[F, A] =
        ConsequenceT(F.point(Consequence.success(a)))

      def bind[A, B](fa: ConsequenceT[F, A])(f: A => ConsequenceT[F, B]): ConsequenceT[F, B] =
        ConsequenceT {
          F.bind(fa.run) {
            case Consequence.Success(a, _) => f(a).run
            case Consequence.Error(c) => F.point(Consequence.Error(c))
          }
        }
    }
}
