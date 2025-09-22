package org.goldenport.scalaz

import scala.language.higherKinds
import scalaz._, Scalaz._
import org.goldenport.context.Consequence

/*
 * @since   May. 19, 2025
 *  version May. 19, 2025
 * @version Sep. 19, 2025
 * @author  ASAMI, Tomoharu
 */
package object rwscr {
  type RWSCR[R, S, A] = ReaderWriterStateT[Consequence, R, Recorder, S, A]

  def unit[R, S]: RWSCR[R, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Recorder.empty, (), s))
  }

  def ask[R, S]: RWSCR[R, S, R] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Recorder.empty, r, s))
  }

  def ask[R, S, A](f: R => A): RWSCR[R, S, A] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Recorder.empty, f(r), s))
  }

  def tell[R, S, A](w: Recorder): RWSCR[R, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((w, (), s))
  }

  def get[R, S, A]: RWSCR[R, S, S] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence.success((Recorder.empty, s, s))
  }

  def set[R, S](s: S): RWSCR[R, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence((Recorder.empty, (), s))
  }

  def modify[R, S](f: S => S): RWSCR[R, S, Unit] = ReaderWriterStateT { (r: R, s: S) =>
    Consequence((Recorder.empty, (), f(s)))
  }

  def lift[R, S, A](ca: Consequence[A]): RWSCR[R, S, A] =
    ReaderWriterStateT { (r: R, s: S) =>
      ca.map(a => (Recorder.empty, a, s))
    }
}
