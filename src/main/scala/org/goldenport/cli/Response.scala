package org.goldenport.cli

/*
 * @since   Oct.  6, 2018
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait Response {
  def text: String
}

case class ErrorResponse(
  code: Int,
  message: String,
  detail: Option[Int] = None
) extends Response {
  def text = message
}

case object VoidResponse extends Response {
  val text = ""
}

case class StringResponse(
  string: String
) extends Response {
  def text = string
}

object Response {
  val empty = VoidResponse

  def apply(): Response = VoidResponse

  def apply(p: String): Response = StringResponse(p)

  def notFound(msg: String): Response = ErrorResponse(404, msg)
}
