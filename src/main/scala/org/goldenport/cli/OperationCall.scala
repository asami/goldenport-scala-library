package org.goldenport.cli

import org.goldenport.cli.spec.Operation

/*
 * @since   Oct.  6, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class OperationCall(
  environment: Environment,
  specification: Operation,
  request: Request,
  response: Response
) {
  def isVerbose = request.isVerbose
}

object OperationCall {
  def create(op: Operation, args: Array[String]): OperationCall = {
    val req = Request.create(op, args)
    val res = Response()
    OperationCall(
      Environment.create(op.name, args),
      op,
      req,
      res
    )
  }
}
