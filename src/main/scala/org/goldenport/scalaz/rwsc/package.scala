package org.goldenport.scalaz

import scala.language.higherKinds
import scalaz._, Scalaz._
import org.goldenport.context.Consequence

/*
 * @since   May. 19, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
package object rwsc {
  type RWSC[R, W, S, A] = ReaderWriterStateT[Consequence, R, W, S, A]

  def unit[R, W: Monoid, S]: RWSC[R, W, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Monoid[W].zero, (), s))
  }

  def ask[R, W: Monoid, S]: RWSC[R, W, S, R] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Monoid[W].zero, r, s))
  }

  def ask[R, W: Monoid, S, A](f: R => A): RWSC[R, W, S, A] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Monoid[W].zero, f(r), s))
  }

  def tell[R, W: Monoid, S, A](w: W): RWSC[R, W, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((w, (), s))
  }

  def get[R, W: Monoid, S, A]: RWSC[R, W, S, S] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Monoid[W].zero, s, s))
  }

  def set[R, W: Monoid, S](s: S): RWSC[R, W, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence((Monoid[W].zero, (), s))
  }

  def modify[R, W: Monoid, S](f: S => S): RWSC[R, W, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence((Monoid[W].zero, (), f(s)))
  }
}
