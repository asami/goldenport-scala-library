package org.goldenport.scalaz

import scalaz._, Scalaz._

/*
 * @since   May. 19, 2025
 * @version May. 19, 2025
 * @author  ASAMI, Tomoharu
 */
case class Recorder(
) {
  def +(rhs: Recorder): Recorder = ???
}

object Recorder {
  val empty = Recorder()

  implicit object RecorderMonoid extends Monoid[Recorder] {
    def zero = empty
    def append(lhs: Recorder, rhs: => Recorder) = lhs + rhs
  }
}
