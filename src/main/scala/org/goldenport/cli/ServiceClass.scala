package org.goldenport.cli

import org.goldenport.RAISE

/*
 * @since   Feb. 18, 2019
 *  version Feb. 24, 2019
 *  version Feb. 13, 2020
 *  version Dec. 18, 2021
 * @version Nov. 25, 2023
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

  def makeRequest(req: Request, args: Seq[String]): Option[Either[Response, Request]] =
    specification.makeRequest(req, args)

  def accept(req: Request): Option[Operation] =
    if (specification.accept(req))
      Some(operation(req))
    else
      None

  def execute(env: Environment, args: Seq[String]): Response = {
    val a = args.toList match {
      case Nil => defaultOperation match {
        case Some(s) => Some(Right(Request.create(s.specification, Array[String]())))
        case None => RAISE.notImplementedYetDefect // TODO
      }
      case x :: xs => operations.makeRequest(Request(x), xs)
    }
    a match {
      case Some(s) => s match {
        case Right(req) =>
          val op = operation(req)
          op.apply(env, req)
        case Left(res) => res
      }
      case None => ???
    }
  }
}
