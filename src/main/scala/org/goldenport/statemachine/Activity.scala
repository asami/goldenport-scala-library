package org.goldenport.statemachine

/*
 * @since   May.  2, 2021
 * @version May.  2, 2021
 * @author  ASAMI, Tomoharu
 */
trait Activity {
  def apply(p: Parcel): Parcel
}

object Activity {
}

case object NoneActivity extends Activity {
  def apply(p: Parcel): Parcel = p
}
