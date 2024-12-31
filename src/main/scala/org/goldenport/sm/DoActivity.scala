package org.goldenport.sm

/*
 * @since   May.  2, 2021
 *  version May.  3, 2021
 *  version Nov. 28, 2021
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
trait DoActivity {
  def entry(sm: StateMachine, p: Parcel): Parcel
  def exit(sm: StateMachine, p: Parcel): Parcel
}

object DoActivity {
  case object Empty extends DoActivity {
    def entry(sm: StateMachine, p: Parcel): Parcel = p
    def exit(sm: StateMachine, p: Parcel): Parcel = p
  }

  case class EntryExit(
    entryActivity: Activity,
    exitActivity: Activity
  ) extends DoActivity {
    def entry(sm: StateMachine, p: Parcel): Parcel = entryActivity(sm, p)
    def exit(sm: StateMachine, p: Parcel): Parcel = exitActivity(sm, p)
  }

  def apply(entry: Activity, exit: Activity): DoActivity = EntryExit(entry, exit)
}
