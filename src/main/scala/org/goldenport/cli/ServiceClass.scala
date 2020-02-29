package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 *  version Feb. 24, 2019
 * @version Feb. 13, 2020
 * @author  ASAMI, Tomoharu
 */
trait ServiceClass {
  def name: String
  def defaultOperation: Option[OperationClass]
  def operations: Operations

  def specification: spec.Service = spec.Service(
    name,
    default_operation_spec,
    operation_specs
  )

  protected final def default_operation_spec = defaultOperation.map(_.specification)

  protected final def operation_specs = operations.operations.map(_.specification)

  def operation(req: Request): Operation = {
    val r = req.operation match {
      case spec.Operation.OP_DEFAULT => defaultOperation.map(_.operation(req)).getOrElse(???)
      case m => operations.get(req).getOrElse(???)
    }
    // println(s"$name: $req => $r")
    r
  }

  def makeRequest(command: String, args: Seq[String]): Option[Request] =
    specification.makeRequest(command, args)

  def accept(req: Request): Option[Operation] =
    if (specification.accept(req))
      Some(operation(req))
    else
      None
}
