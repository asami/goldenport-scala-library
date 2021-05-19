package org.goldenport.cli

import java.net.URL

/*
 * @since   Oct.  6, 2018
 *  version Oct. 10, 2018
 *  version Feb. 18, 2019
 * @version Apr. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class OperationCall(
  environment: Environment,
  specification: spec.Operation,
  request: Request,
  response: Response
) {
  def isVerbose = request.isVerbose
  def argumentsAsString: List[String] = request.argumentsAsString
  def argumentsAsUrl: List[URL] = request.argumentsAsUrl
  def asUrlList(p: Symbol): List[URL] = request.asUrlList(p)
}

object OperationCall {
  def create(op: spec.Operation, args: Array[String]): OperationCall = {
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
