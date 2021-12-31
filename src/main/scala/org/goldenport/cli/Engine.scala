package org.goldenport.cli

import org.goldenport.RAISE
import org.goldenport.parser.CommandParser

/*
 * @since   Feb. 17, 2019
 *  version Feb. 24, 2019
 *  version Oct. 14, 2019
 *  version Feb. 18, 2020
 *  version Mar.  1, 2020
 *  version May. 16, 2020
 *  version Apr. 25, 2021
 * @version Dec. 18, 2021
 * @author  ASAMI, Tomoharu
 */
case class Engine(
  config: Engine.Config,
  services: Services,
  operations: Operations
) {
  import Engine._

  val commandParser: CommandParser[Candidate] = {
    val a = services.services.map(ServiceCandidate)
    val b = operations.operations.map(OperationCandidate)
    val xs = (a ++ b).map(x => x.name -> x)
    CommandParser.create(xs)
  }

  def apply(env: Environment, command: String, args: Seq[String]): Response = {
    val qcommand = _resolve_command(command)
    val op = services.makeRequest(qcommand, args) orElse operations.makeRequest(command, args)
    val r = op.map(apply(env, _)).
      getOrElse(Response.notFound(command, _get_candidates(command)))
    _output(env, r)
  }

  def apply(service: String, command: String, args: Seq[String]): Response = {
    RAISE.notImplementedYetDefect
  }

  def apply(env: Environment, req: Request): Response = {
    // TODO candidates
    val op = services.get(req) orElse operations.get(req)
    val r = op.map(_.apply(env, req)).
      getOrElse(Response.notFound(req.name, _get_candidates(req)))
    _output(env, r)
  }

  def apply(env: Environment, args: Seq[String]): Response = {
    val r = args.toList match {
      case Nil => RAISE.notImplementedYetDefect // TODO help
      case x :: xs =>
        commandParser(x) match {
          case m: CommandParser.NotFound[Candidate] => Response.notFound(x, _get_candidates(x))
          case m: CommandParser.Found[Candidate] => m.command match {
            case ServiceCandidate(service) => apply(env, service, xs)
            case OperationCandidate(operation) => apply(env, operation, xs)
          }
          case m: CommandParser.Candidates[Candidate] => Response.ambiguous(x, m)
        }
    }
    _output(env, r)
  }

  def apply(env: Environment, service: ServiceClass, args: Seq[String]): Response = {
    val r = service.execute(env, args)
    _output(env, r)
  }

  def apply(env: Environment, operation: OperationClass, args: Seq[String]): Response = {
    val r = operation.execute(env, args)
    _output(env, r)
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

  private def _output(
    env: Environment,
    p: Response
  ): Response = {
    if (config.isOutput)
      p.output(env)
    if (config.isStdout)
      p.stdout.foreach(env.printStdoutLn)
    if (config.isStderr)
      p.stderr.foreach(env.printStderrLn)
    if (config.isExit) {
      System.exit(0) // TODO
    }
    p
  }
}

object Engine {
  case class Config(
    isStdout: Boolean = true,
    isStderr: Boolean = true,
    isOutput: Boolean = true,
    isExit: Boolean = false
  ) {
  }
  object Config {
    val default = Config()
    val terse = default.copy(isStdout = false, isStderr = false, isOutput = false)
  }

  sealed trait Candidate {
    def name: String
  }

  case class ServiceCandidate(service: ServiceClass) extends Candidate {
    def name = service.name
  }

  case class OperationCandidate(operation: OperationClass) extends Candidate {
    def name = operation.name
  }

  def standard(services: Services, operations: Operations): Engine =
    Engine(Config.default, services, operations)

  def terse(services: Services, operations: Operations): Engine =
    Engine(Config.terse, services, operations)
//  def apply(services: Services): Engine = Engine(services, Operations.empty)
}
