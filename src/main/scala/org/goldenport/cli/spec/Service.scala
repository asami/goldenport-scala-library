package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest}

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 *  version Feb. 24, 2019
 * @version Feb. 13, 2020
 * @author  ASAMI, Tomoharu
 */
case class Service(
  name: String,
  defaultOperation: Option[Operation],
  operations: Seq[Operation]
) {
  def accept(req: CliRequest): Boolean =
    req.service.map(_ == name).getOrElse(req.operation == name)

  def makeRequest(command: String, args: Seq[String]): Option[CliRequest] = {
    val req = CliRequest(command)
    val op: Option[Operation] = req.service.
      flatMap(s => operations.find(_.name == req.operation)).
      orElse(if (req.operation == name) defaultOperation else None)
    val d = op.map { x =>
      val (r, remainders) = x.parse(args)
      r
    }.map(_.withService(name))
    // println("XXX" + d)
    d
  }
}

object Service {
}
