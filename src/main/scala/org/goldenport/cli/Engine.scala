package org.goldenport.cli

import org.goldenport.RAISE
import org.goldenport.parser.CommandParser

/*
 * @since   Feb. 17, 2019
 *  version Feb. 24, 2019
 * @version Oct. 14, 2019
 * @author  ASAMI, Tomoharu
 */
case class Engine(services: Services, operations: Operations) {
  import Engine._

  val commandParser: CommandParser[Candidate] = {
    val a = services.services.map(ServiceCandidate)
    val b = operations.operations.map(OperationCandidate)
    val xs = (a ++ b).map(x => x.name -> x)
    CommandParser.create(xs)
  }

  def apply(env: Environment, command: String, args: List[String]): Response = {
    val qcommand = _resolve_command(command)
    val op = services.makeRequest(qcommand, args) orElse operations.makeRequest(command, args)
    op.map(apply(env, _)).
      getOrElse(Response.notFound(command, _get_candidates(command)))
  }

  def apply(service: String, command: String, args: List[String]): Response = {
    RAISE.notImplementedYetDefect
  }

  def apply(env: Environment, req: Request): Response = {
    // TODO candidates
    val op = services.get(req) orElse operations.get(req)
    op.map(_.apply(env, req)).
      getOrElse(Response.notFound(req.name, _get_candidates(req)))
  }

  private def _resolve_command(name: String): String = {
    val cmd = commandParser(name)
    cmd match {
      case CommandParser.Found(s) => s.name
      case _ => name
    }
  }

  private def _get_candidates(req: Request): Option[CommandParser.Candidates[Candidate]] =
    _get_candidates(req.service getOrElse req.operation)

  private def _get_candidates(name: String): Option[CommandParser.Candidates[Candidate]] =
    commandParser(name) match {
      case m: CommandParser.Candidates[_] => Some(m.asInstanceOf[CommandParser.Candidates[Candidate]])
      case _ => None
    }
}

object Engine {
  sealed trait Candidate {
    def name: String
  }

  case class ServiceCandidate(service: ServiceClass) extends Candidate {
    def name = service.name
  }

  case class OperationCandidate(operation: OperationClass) extends Candidate {
    def name = operation.name
  }
}
