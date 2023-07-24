package org.goldenport.cli

import java.net.URL
import org.goldenport.context._

/*
 * @since   Oct.  6, 2018
 *  version Oct. 10, 2018
 *  version Feb. 18, 2019
 *  version Apr. 25, 2021
 * @version Jul. 23, 2023
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

  def getArg1AsString: Option[String] = request.getArg1AsString
  def getArg1AsIntOrString: Option[Either[Int, String]] = request.getArg1AsIntOrString

  def consequenceArg1ListingDirectiveOption: Consequence[Option[ListingDirective]] = request.consequenceArg1ListingDirectiveOption
  def consequenceArg1ListingDirectiveBaseOneOption: Consequence[Option[ListingDirective]] = request.consequenceArg1ListingDirectiveBaseOneOption
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
