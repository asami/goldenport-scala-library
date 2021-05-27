package org.goldenport.statemachine

/*
 * @since   May.  2, 2021
 * @version May.  3, 2021
 * @author  ASAMI, Tomoharu
 */
trait DoActivity {
  def entry(p: Parcel): Parcel
  def exit(p: Parcel): Parcel
}

object DoActivity {
}

case object NoneDoActivity extends DoActivity {
  def entry(p: Parcel): Parcel = p
  def exit(p: Parcel): Parcel = p
}
