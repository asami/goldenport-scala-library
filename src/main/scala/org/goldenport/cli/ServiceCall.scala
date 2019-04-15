package org.goldenport.cli

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 * @version Feb. 18, 2019
 * @author  ASAMI, Tomoharu
 */
case class ServiceCall(
  environment: Environment,
  specification: spec.Service,
  request: Request,
  response: Response
) {
}

object ServiceCall {
}
