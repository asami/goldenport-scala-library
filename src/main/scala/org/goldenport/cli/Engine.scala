package org.goldenport.cli

import org.goldenport.RAISE

/*
 * @since   Feb. 17, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
case class Engine(services: Services, operations: Operations) {
  def apply(env: Environment, command: String, args: List[String]): Response = {
    val op = services.makeRequest(command, args) orElse operations.makeRequest(command, args)
    op.map(apply(env, _)).
      getOrElse(Response.notFound(command))
  }

  def apply(service: String, command: String, args: List[String]): Response = {
    RAISE.notImplementedYetDefect
  }

  def apply(env: Environment, req: Request): Response = {
    val op = services.get(req) orElse operations.get(req)
    op.map(_.apply(env, req)).
      getOrElse(Response.notFound(req.name))
  }
  
}
