package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest, Environment}

/*
 * @since   Oct.  6, 2018
 *  version Oct. 10, 2018
 *  version Feb. 24, 2019
 * @version Feb. 15, 2020
 * @author  ASAMI, Tomoharu
 */
case class Operation(
  name: String,
  request: Request,
  response: Response
) {
  def accept(req: CliRequest): Boolean = req.operation == name

  def makeRequest(command: String, args: Seq[String]): Option[CliRequest] =
    if (name == command)
      Some(parse(args)._1)
    else
      None

  def parse(args: Seq[String]): (CliRequest, List[String]) = parse(CliRequest(name), args)

  def parse(req: CliRequest, args: Seq[String]): (CliRequest, List[String]) =
    request.parse(req, args)

  def buildRequest(env: Environment, args: Seq[Any]): CliRequest = {
    request.build(CliRequest(name), args)
  }
}

object Operation {
  val OP_DEFAULT = "default"

  val default: Operation = default(OP_DEFAULT)
  def default(op: String): Operation = Operation(op, Request.empty, Response.string)
}
