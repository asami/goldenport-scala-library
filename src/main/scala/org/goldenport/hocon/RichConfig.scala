package org.goldenport.hocon

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scala.util.Try
                    import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import java.net.{URL, URI}
import com.typesafe.config._
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NElement}

/*
 * Migrated from org.goldenport.hocon.HoconUtils.
 * See TypesafeConfigUtils.
 *
 * @since   Nov. 17, 2016
 *  version Jun. 23, 2017
 *  version Aug. 29, 2017
 *  version Oct. 27, 2017
 *  version Dec. 14, 2017
 *  version Oct. 21, 2018
 * @version Nov. 19, 2018
 * @author  ASAMI, Tomoharu
 */
case class RichConfig(config: Config) extends AnyVal {
  def withFallback(p: RichConfig): RichConfig = RichConfig(config.withFallback(p.config))
  def isDefined(key: String): Boolean = HoconUtils.isDefined(config, key)
  def asBoolean(key: String, fallback: Boolean): Boolean = HoconUtils.asBoolean(config, key, fallback)
  def asStringList(key: String) = HoconUtils.asStringList(config, key)
  def asEagerStringList(key: String) = HoconUtils.asEagerStringList(config, key)
  def asStringVector(key: String) = HoconUtils.asStringVector(config, key)
  def asEagerStringVector(key: String) = HoconUtils.asEagerStringVector(config, key)
  def asUrlList(key: String) = HoconUtils.asUrlList(config, key)
  def asDuration(key: String, fallback: FiniteDuration) = HoconUtils.asDuration(config, key, fallback)
  def takeI18NString(key: String) = HoconUtils.takeI18NString(config, key)
  def takeI18NElement(key: String) = HoconUtils.takeI18NElement(config, key)
  def getObjectOption(key: String) = HoconUtils.getObject(config, key)
  def getStringOption(key: String) = HoconUtils.getString(config, key)
  def getBooleanOption(key: String) = HoconUtils.getBoolean(config, key)
  def getIntOption(key: String)= HoconUtils.getInt(config, key)
  def getDurationOption(key: String) = HoconUtils.getDuration(config, key)
  def getDurationByMinuteOption(key: String) = HoconUtils.getDurationByMinute(config, key)
  def getStringListOption(key: String) = HoconUtils.getStringList(config, key)
  def getEagerStringListOption(key: String) = HoconUtils.getEagerStringList(config, key)
  def getNonEmptyListStringOption(key: String) = HoconUtils.getNonEmptyListString(config, key)
  def getEagerNonEmptyListStringOption(key: String) = HoconUtils.getEagerNonEmptyListString(config, key)
  def getUriOption(key: String) = HoconUtils.getUri(config, key)
  def getUriListOption(key: String) = HoconUtils.getUriList(config, key)
  def getI18NStringOption(key: String) = HoconUtils.getI18NString(config, key)
  def getI18NElementOption(key: String) = HoconUtils.getI18NElement(config, key)
  def getConfigOption(key: String) = HoconUtils.getConfig(config, key)
  def +(rhs: RichConfig): RichConfig = new RichConfig(rhs.config.withFallback(config))
}

object RichConfig {
  val empty = RichConfig(ConfigFactory.empty())

  implicit object RichConfigMonoid extends Monoid[RichConfig] {
    def zero = empty
    def append(lhs: RichConfig, rhs: => RichConfig) = lhs + rhs
  }

  object Implicits {
    implicit def enrichConfig(config: Config) = new RichConfig(config)
  }
}