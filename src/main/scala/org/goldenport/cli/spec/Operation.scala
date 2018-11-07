package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest}

/*
 * @since   Oct.  6, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Operation(
  name: String,
  request: Request,
  response: Response
) {
  def parse(req: CliRequest, args: List[String]): (CliRequest, List[String]) =
    request.parse(req, args)
}

object Operation {
}
