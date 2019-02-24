package org.goldenport.cli

import org.goldenport.util.AnyRefUtils
import Environment.AppEnvironment

/*
 * @since   Feb. 24, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait Method {
  def call: OperationCall
  def run: Response = execute
  def execute: Response

  def environment = call.environment
  def toAppEnvironment[T <: AppEnvironment]: T = environment.toAppEnvironment

  protected final def head_string_option: Option[String] =
    call.request.arguments.headOption.map(x => AnyRefUtils.toString(x.value))

  protected final def to_response(p: String) = StringResponse(p)
}
