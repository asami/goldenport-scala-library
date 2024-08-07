package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 *  version Feb. 24, 2019
 *  version Feb. 13, 2020
 * @version Nov. 25, 2023
 * @author  ASAMI, Tomoharu
 */
case class Services(services: Vector[ServiceClass]) {
  def makeRequest(req: Request, args: Seq[String]): Option[Either[Response, Request]] =
    services.toStream.flatMap(_.makeRequest(req, args)).headOption

  def get(req: Request): Option[Operation] = {
    services.toStream.flatMap(_.accept(req)).headOption
  }
}

object Services {
  val empty = Services(Vector.empty)

  def apply(p: ServiceClass, ps: ServiceClass*): Services = Services((p +: ps).toVector)
}
