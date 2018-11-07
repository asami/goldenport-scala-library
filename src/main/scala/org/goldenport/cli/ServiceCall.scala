package org.goldenport.cli

import org.goldenport.cli.spec.Service

/*
 * @since   Oct.  6, 2018
 * @version Oct.  8, 2018
 * @author  ASAMI, Tomoharu
 */
case class ServiceCall(
  environment: Environment,
  specification: Service,
  request: Request,
  response: Response
) {
}

object ServiceCall {
}
