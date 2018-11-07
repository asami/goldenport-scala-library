package org.goldenport.cli

/*
 * @since   Oct.  5, 2018
 * @version Oct.  6, 2018
 * @author  ASAMI, Tomoharu
 */
class Environment(
  val config: Config
) {
  def charset = config.charset
  def locale = config.locale
  def timezone = config.timezone
}

object Environment {
  def create(appname: String, args: Array[String]): Environment = {
    val config = Config.build(appname)
    new Environment(config)
  }

  def create(args: Array[String]): Environment = {
    val config = Config.build()
    new Environment(config)
  }
}
