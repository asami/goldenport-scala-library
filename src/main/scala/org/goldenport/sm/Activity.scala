package org.goldenport.sm

/*
 * @since   May.  2, 2021
 *  version Nov. 28, 2021
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
trait Activity {
  def apply(sm: StateMachine, p: Parcel): Parcel
}

object Activity {
  case object Empty extends Activity {
    def apply(sm: StateMachine, p: Parcel): Parcel = p
  }

  case class Opaque(script: String) extends Activity {
    def apply(sm: StateMachine, p: Parcel): Parcel = p.execute(sm, this)
  }
}
