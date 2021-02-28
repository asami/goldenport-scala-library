package org.goldenport.context

/*
 * @since   Feb. 21, 2021
 * @version Feb. 22, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Fault {
}

sealed trait ArgumentFault {
}

sealed trait IoFault {
}

case class Faults(faults: List[Fault] = Nil) {
}
object Faults {
  val empty = Faults()
}
