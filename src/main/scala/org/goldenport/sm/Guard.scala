package org.goldenport.sm

/*
 * @since   May.  3, 2021
 *  version May. 29, 2021
 *  version Jun. 13, 2021
 *  version Sep.  3, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
trait SmGuard {
  def isGoAhead: Boolean = false
  def accept(p: Parcel): Boolean
}

object Guard {
  // case class All() extends Guard {
  // }
}

case object AllGuard extends SmGuard {
  override def isGoAhead = true
  def accept(p: Parcel): Boolean = true
}

// In case of empty, allways false.
case class AndGuard(exprs: Vector[SmGuard]) extends SmGuard {
  def accept(p: Parcel): Boolean =
    if (exprs.isEmpty)
      false
    else
      exprs.forall(_.accept(p))
}

// In case of empty, allways false.
case class OrGuard(exprs: Vector[SmGuard]) extends SmGuard {
  def accept(p: Parcel): Boolean = exprs.exists(_.accept(p))
}

case class EventNameGuard(name: String) extends SmGuard {
  def accept(p: Parcel): Boolean = p.event.name == name
}

case class ResourceIdGuard(resourceId: String) extends SmGuard {
  def accept(p: Parcel): Boolean = p.event.isResourceId(resourceId)
}

case class ToStateGuard(name: String, value: Option[Int]) extends SmGuard {
  def accept(p: Parcel): Boolean =
    p.event.name == name || value.fold(false)(_.toString == p.event.name)
}
object ToStateGuard {
  def apply(name: String, value: Int): ToStateGuard = ToStateGuard(name, Some(value))
}
