package org.goldenport.cli

import org.goldenport.RAISE

/*
 * @since   Feb. 18, 2019
 *  version Feb. 21, 2019
 *  version Feb. 13, 2020
 * @version Nov. 25, 2023
 * @author  ASAMI, Tomoharu
 */
case class Operations(operations: Vector[OperationClass]) {
  def makeRequest(req: Request, args: Seq[String]): Option[Either[Response, Request]] =
    operations.toStream.flatMap(_.makeRequest(req, args)).headOption

  def get(req: Request): Option[Operation] = {
    operations.toStream.flatMap(_.accept(req)).headOption
  }
}

object Operations {
  val empty = Operations(Vector.empty)

  def apply(p: OperationClass, ps: OperationClass*): Operations = Operations((p +: ps).toVector)
}
