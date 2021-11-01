package org.goldenport.event

/*
 * @since   Oct. 31, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
case class ObjectId(
  id: String,
  entity: Option[String]
) {
}

object ObjectId {
  def apply(id: String): ObjectId = ObjectId(id, None)

  def apply(id: String, entity: String): ObjectId = ObjectId(id, Some(entity))
}
