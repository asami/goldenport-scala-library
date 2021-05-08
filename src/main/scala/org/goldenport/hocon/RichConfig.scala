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
import org.goldenport.collection.VectorMap
import org.goldenport.value._
import org.goldenport.parser.ParseResult

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
 *  version Nov. 19, 2018
 *  version Mar. 24, 2019
 *  version Apr. 28, 2019
 *  version Jun. 18, 2020
 *  version Apr. 24, 2021
 *  version Apr. 24, 2021
 * @version May.  5, 2021
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
  def takeString(key: String): String = HoconUtils.takeString(config, key)
  def takeLocale(key: String) = HoconUtils.takeLocale(config, key)
  def takeI18NString(key: String) = HoconUtils.takeI18NString(config, key)
  def takeI18NElement(key: String) = HoconUtils.takeI18NElement(config, key)
  def takeConfig(key: String) = HoconUtils.takeConfig(config, key)
  def takeConfigList(key: String) = HoconUtils.takeConfigList(config, key)
  def takeValue[T <: ValueInstance](valueclass: ValueClass[T], key: String) = HoconUtils.takeValue(valueclass, config, key)
  def asValue[T <: ValueInstance](valueclass: ValueClass[T], key: String, default: T) = HoconUtils.takeValue(valueclass, config, key, default)
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
  def getLocaleOption(key: String) = HoconUtils.getLocale(config, key)
  def getUriOption(key: String) = HoconUtils.getUri(config, key)
  def getUriListOption(key: String) = HoconUtils.getUriList(config, key)
  def getI18NStringOption(key: String) = HoconUtils.getI18NString(config, key)
  def getI18NElementOption(key: String) = HoconUtils.getI18NElement(config, key)
  def getConfigOption(key: String) = HoconUtils.getConfig(config, key)
  def getRichConfigOption(key: String) = HoconUtils.getRichConfig(config, key)
  def getValueOption[T <: ValueInstance](valueclass: ValueClass[T], key: String) = HoconUtils.getValue(valueclass, config, key)

  def parseString(key: String): ParseResult[String] = HoconUtils.parseString(config, key)
  def parseStringOption(key: String): ParseResult[Option[String]] = HoconUtils.parseStringOption(config, key)
  def parseStringOrConfig(key: String): ParseResult[Either[String, Config]] = HoconUtils.parseStringOrConfig(config, key)
  def parseStringOrConfigOption(key: String): ParseResult[Option[Either[String, Config]]] = HoconUtils.parseStringOrConfigOption(config, key)
  def parseConfig(key: String): ParseResult[Config] = HoconUtils.parseConfig(config, key)
  def parseConfigList(key: String): ParseResult[List[Config]] = HoconUtils.parseConfigList(config, key)
  def parseAsConfigList(key: String): ParseResult[List[Config]] = HoconUtils.parseAsConfigList(config, key)
  def parseObjectList[T](key: String, f: Config => ParseResult[T]): ParseResult[List[T]] = HoconUtils.parseObjectList(config, key, f)
  def parseAsObjectList[T](key: String, f: Config => ParseResult[T]): ParseResult[List[T]] = HoconUtils.parseAsObjectList(config, key, f)

  def childConfigMap: Map[String, Config] = HoconUtils.childConfigMap(config)
  def childRichConfigMap: Map[String, RichConfig] = HoconUtils.childRichConfigMap(config)
  def buildMap[T](f: Config => Option[T]): VectorMap[String, T] = HoconUtils.buildMap(config, f)
  def +(rhs: RichConfig): RichConfig = new RichConfig(rhs.config.withFallback(config))
}

object RichConfig {
  def empty() = RichConfig(ConfigFactory.empty())

  implicit object RichConfigMonoid extends Monoid[RichConfig] {
    def zero = empty
    def append(lhs: RichConfig, rhs: => RichConfig) = lhs + rhs
  }

  object Implicits {
    implicit def enrichConfig(config: Config) = new RichConfig(config)
  }
}
