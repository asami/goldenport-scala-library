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
package object rwscrt {
  type RWSCRT[T[_], R, W, S, A] = ReaderWriterStateT[({ type L[X] = ConsequenceT[T, X] })#L, R, W, S, A]

  def unit[T[_]: Monad, R, S]: RWSCRT[T, R, Recorder, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, Unit] { (r: R, s: S) =>
    val result: (Recorder, Unit, S) = (Recorder.empty, (), s)
    val c: Consequence[(Recorder, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def ask[T[_]: Monad, R, S]: RWSCRT[T, R, Recorder, S, R] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, R] { (r: R, s: S) =>
    val result: (Recorder, R, S) = (Recorder.empty, r, s)
    val c: Consequence[(Recorder, R, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, R, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def ask[T[_]: Monad, R, S, A](f: R => A): RWSCRT[T, R, Recorder, S, A] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, A] { (r: R, s: S) =>
    val result: (Recorder, A, S) = (Recorder.empty, f(r), s)
    val c: Consequence[(Recorder, A, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, A, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def tell[T[_]: Monad, R, S](w: Recorder): RWSCRT[T, R, Recorder, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, Unit] { (r: R, s: S) =>
    val result: (Recorder, Unit, S) = (w, (), s)
    val c: Consequence[(Recorder, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def get[T[_]: Monad, R, S]: RWSCRT[T, R, Recorder, S, S] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, S] { (r: R, s: S) =>
    val result: (Recorder, S, S) = (Recorder.empty, s, s)
    val c: Consequence[(Recorder, S, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, S, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def set[T[_]: Monad, R, S](s: S): RWSCRT[T, R, Recorder, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, Unit] { (r: R, _: S) =>
    val result: (Recorder, Unit, S) = (Recorder.empty, (), s)
    val c: Consequence[(Recorder, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }

  def modify[T[_]: Monad, R, S](f: S => S): RWSCRT[T, R, Recorder, S, Unit] = ReaderWriterStateT[({ type λ[α] = ConsequenceT[T, α] })#λ, R, Recorder, S, Unit] { (r: R, s: S) =>
    val result: (Recorder, Unit, S) = (Recorder.empty, (), f(s))
    val c: Consequence[(Recorder, Unit, S)] = Consequence.success(result)
    val lifted: T[Consequence[(Recorder, Unit, S)]] = Monad[T].point(c)
    ConsequenceT(lifted)    
  }
}
