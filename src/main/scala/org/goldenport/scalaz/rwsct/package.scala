package org.goldenport.scalaz

import scala.language.higherKinds
import scalaz._, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.context.ConsequenceT

/*
 * @since   May. 19, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
package object rwsct {
  type RWSCT[T[_], R, W, S, A] = ReaderWriterStateT[({ type L[X] = ConsequenceT[T, X] })#L, R, W, S, A]

  def unit[T[_]: Monad, R, W: Monoid, S]: RWSCT[T, R, W, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, Unit] { (r: R, s: S) =>
    val result: (W, Unit, S) = (Monoid[W].zero, (), s)
    val c: Consequence[(W, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def ask[T[_]: Monad, R, W: Monoid, S]: RWSCT[T, R, W, S, R] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, R] { (r: R, s: S) =>
    val result: (W, R, S) = (Monoid[W].zero, r, s)
    val c: Consequence[(W, R, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, R, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def ask[T[_]: Monad, R, W: Monoid, S, A](f: R => A): RWSCT[T, R, W, S, A] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, A] { (r: R, s: S) =>
    val result: (W, A, S) = (Monoid[W].zero, f(r), s)
    val c: Consequence[(W, A, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, A, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def tell[T[_]: Monad, R, W: Monoid, S](w: W): RWSCT[T, R, W, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, Unit] { (r: R, s: S) =>
    val result: (W, Unit, S) = (w, (), s)
    val c: Consequence[(W, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def get[T[_]: Monad, R, W: Monoid, S]: RWSCT[T, R, W, S, S] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, S] { (r: R, s: S) =>
    val result: (W, S, S) = (Monoid[W].zero, s, s)
    val c: Consequence[(W, S, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, S, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def set[T[_]: Monad, R, W: Monoid, S](s: S): RWSCT[T, R, W, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, Unit] { (r: R, _: S) =>
    val result: (W, Unit, S) = (Monoid[W].zero, (), s)
    val c: Consequence[(W, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def modify[T[_]: Monad, R, W: Monoid, S](f: S => S): RWSCT[T, R, W, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, W, S, Unit] { (r: R, s: S) =>
    val result: (W, Unit, S) = (Monoid[W].zero, (), f(s))
    val c: Consequence[(W, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(W, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }
}
