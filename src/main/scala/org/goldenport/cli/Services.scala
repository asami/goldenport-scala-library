package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
case class Services(services: Vector[ServiceClass]) {
  def makeRequest(command: String, args: List[String]): Option[Request] =
    services.toStream.flatMap(_.makeRequest(command, args)).headOption

  def get(req: Request): Option[Operation] = {
    services.toStream.flatMap(_.accept(req)).headOption
  }
}

object Services {
  val empty = Services(Vector.empty)

  def apply(p: ServiceClass, ps: ServiceClass*): Services = Services((p +: ps).toVector)
}
