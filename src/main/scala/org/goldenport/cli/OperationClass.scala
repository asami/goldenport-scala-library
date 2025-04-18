package org.goldenport.cli

/*
 * @since   Feb. 18, 2019
 *  version Oct. 13, 2019
 *  version Feb. 16, 2020
 *  version Nov. 25, 2023
 * @version Mar. 15, 2025
 * @author  ASAMI, Tomoharu
 */
trait OperationClass {
  def name = specification.name
  def specification: spec.Operation

  def operation(req: Request): Operation

  def makeRequest(req: Request, args: Seq[String]): Option[Either[Response, Request]] =
    specification.makeRequest(req, args)

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
