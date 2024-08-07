package org.goldenport.cli.spec

import org.goldenport.parser.CommandParser
import org.goldenport.cli.{Request => CliRequest}
import org.goldenport.cli.{Response => CliResponse}
import org.goldenport.cli.Engine.Candidate

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 *  version Feb. 24, 2019
 *  version Feb. 13, 2020
 * @version Nov. 25, 2023
 * @author  ASAMI, Tomoharu
 */
case class Service(
  name: String,
  defaultOperation: Option[Operation],
  operations: Seq[Operation]
) {
  import Service._

  val commandParser: CommandParser[Operation] = {
    val xs = operations.map(x => x.name -> x)
    CommandParser.create(xs)
  }

  def accept(req: CliRequest): Boolean =
    req.service.map(_ == name).getOrElse(req.operation == name)

  def makeRequest(req: CliRequest, args: Seq[String]): Option[Either[CliResponse, CliRequest]] = {
    // val req = CliRequest(command)
    // val op: Option[Operation] = req.service.
    //   flatMap(s => operations.find(_.name == req.operation)).
    //   orElse(if (req.operation == name) defaultOperation else None)
    // val d = op.map { x =>
    //   val (r, remainders) = x.parse(args)
    //   r
    // }.map(_.withService(name))
    // // println("XXX" + d)
    // d
    val opname = req.operation
    // println(s"Service#name: $name")
    // println(s"Service#req: $req")
    // println(s"Service#opname: $opname")
    commandParser(opname) match {
      case m: CommandParser.NotFound[Operation] => Some(Left(CliResponse.notFound(opname, _candidates())))
      case m: CommandParser.Found[Operation] =>
        val op = m.command
        val (r, remainders) = op.parse(args)
        Some(Right(r.withService(name)))
      case m: CommandParser.Candidates[Operation] => Some(Left(CliResponse.ambiguous(opname, _candidates(m))))
    }
  }

  private def _candidates(): Seq[String] = operations.map(_.name)

  private def _candidates(p: CommandParser.Candidates[Operation]): Seq[String] =
    p.commands.map(_.name).list
}

object Service {
}
