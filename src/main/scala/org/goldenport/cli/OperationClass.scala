package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 *  version Oct. 13, 2019
 * @version Feb. 16, 2020
 * @author  ASAMI, Tomoharu
 */
trait OperationClass {
  def name = specification.name
  def specification: spec.Operation

  def operation(req: Request): Operation

  def makeRequest(command: String, args: Seq[String]): Option[Request] =
    specification.makeRequest(command, args)

  def accept(req: Request): Option[Operation] =
    if (specification.accept(req))
      Some(operation(req))
    else
      None

  def buildRequest(env: Environment, args: Seq[Any]): Request =
    specification.buildRequest(env, args)

  def execute(env: Environment, args: Seq[Any]): Response = {
    val req = buildRequest(env, args)
    val op = operation(req)
    op(env, req)
  }
}

trait OperationClassWithOperation extends OperationClass with Operation {
    def operation(req: Request): Operation = this
}
