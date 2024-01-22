package org.goldenport.statemachine

import org.goldenport.value._

/*
 * @since   Jun. 13, 2021
 * @version Jun. 14, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait StateMachineKind extends NamedValueInstance {
}

object StateMachineKind extends EnumerationClass[StateMachineKind] {
  val elements = Vector(Plain, Resource)

  case object Plain extends StateMachineKind {
    val name = "plain"
  }
  case object Resource extends StateMachineKind {
    val name = "resource"
  }
}
