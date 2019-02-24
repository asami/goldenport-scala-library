package org.goldenport.cli

import Environment._

/*
 * @since   Oct.  5, 2018
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
case class Environment(
  config: Config,
  service: AppEnvironment = NoneAppEnvironment
) {
  def charset = config.charset
  def locale = config.locale
  def timezone = config.timezone
  def toAppEnvironment[T <: AppEnvironment] = service.asInstanceOf[T]
}

object Environment {
  trait AppEnvironment
  case object NoneAppEnvironment extends AppEnvironment

  def create(appname: String, args: Array[String]): Environment = {
    val config = Config.build(appname)
    new Environment(config)
  }

  def create(args: Array[String]): Environment = {
    val config = Config.build()
    new Environment(config)
  }
}
