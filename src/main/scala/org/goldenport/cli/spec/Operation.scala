package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest}

/*
 * @since   Oct.  6, 2018
 *  version Oct. 10, 2018
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
case class Operation(
  name: String,
  request: Request,
  response: Response
) {
  def accept(req: CliRequest): Boolean = req.operation == name

  def makeRequest(command: String, args: List[String]): Option[CliRequest] =
    if (name == command)
      Some(parse(args)._1)
    else
      None

  def parse(args: List[String]): (CliRequest, List[String]) = parse(CliRequest(name), args)

  def parse(req: CliRequest, args: List[String]): (CliRequest, List[String]) =
    request.parse(req, args)
}

object Operation {
  val OP_DEFAULT = "default"

  val default: Operation = default(OP_DEFAULT)
  def default(op: String): Operation = Operation(op, Request.empty, Response.string)
}
