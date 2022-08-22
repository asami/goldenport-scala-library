package org.goldenport.event

/*
 * @since   Jan.  4, 2021
 *  version May. 23, 2021
 * @version Aug. 22, 2022
 * @author  ASAMI, Tomoharu
 */
trait Signal {
  def name: String
}

object Signal {
  case class NoneSignal() extends Signal {
    def name = "none"
  }

  val none = NoneSignal()
}

// case class Signal(
// ) {
// }

// object Signal {
//   val none = Signal()
// }
