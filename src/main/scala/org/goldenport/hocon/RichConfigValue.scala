package org.goldenport.hocon

import scala.language.implicitConversions
import com.typesafe.config._

/*
 * @since   Nov. 19, 2018
 * @version Nov. 19, 2018
 * @author  ASAMI, Tomoharu
 */
case class RichConfigValue(v: ConfigValue) extends AnyVal {
  def getStringOption(key: String): Option[String] = HoconUtils.getString(v, key)
  def getIntOption(key: String): Option[Int] = HoconUtils.getInt(v, key)
}

object RichConfigValue {
  object Implicits {
    implicit def enrichConfigValue(p: ConfigValue) = new RichConfigValue(p)
  }
}
