package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 *  version Feb. 23, 2019
 * @version Oct. 13, 2019
 * @author  ASAMI, Tomoharu
 */
trait OperationClass {
  def name = specification.name
  def specification: spec.Operation

  def operation(req: Request): Operation

  def makeRequest(command: String, args: List[String]): Option[Request] =
    specification.makeRequest(command, args)

  def accept(req: Request): Option[Operation] =
    if (specification.accept(req))
      Some(operation(req))
    else
      None
}
