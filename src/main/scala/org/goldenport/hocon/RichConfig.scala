package org.goldenport.hocon

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import java.net.{URL, URI}
import java.io.File
import org.joda.time.DateTime
import spire.math.Rational
import com.typesafe.config._
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.collection.VectorMap
import org.goldenport.value._
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
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
 *  version May.  5, 2021
 *  version Jun. 12, 2021
 *  version Oct. 20, 2021
 *  version Dec. 31, 2021
 *  version Jan. 24, 2022
 *  version Feb. 17, 2022
 *  version Mar. 25, 2022
 *  version May. 24, 2022
 *  version Jul. 13, 2022
 *  version Oct. 13, 2022
 * @version Nov. 28, 2022
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
  def asConfig(key: String) = HoconUtils.asConfig(config, key)
  def asConfigList(key: String) = HoconUtils.asConfigList(config, key)
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
  def getBigDecimalOption(key: String) = HoconUtils.getBigDecimal(config, key)
  def getIntOption(key: String)= HoconUtils.getInt(config, key)
  def getDurationOption(key: String) = HoconUtils.getDuration(config, key)
  def getDurationByMinuteOption(key: String) = HoconUtils.getDurationByMinute(config, key)
  def getFiniteDurationOption(key: String) = HoconUtils.getFiniteDuration(config, key)
  def getfiniteDurationByMinuteOption(key: String) = HoconUtils.getFiniteDurationByMinute(config, key)
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
  def getFileOption(key: String): Option[File] = HoconUtils.getFile(config, key)
  def parseBoolean(key: String): ParseResult[Boolean] = HoconUtils.parseBoolean(config, key)
  def parseBooleanOption(key: String): ParseResult[Option[Boolean]] = HoconUtils.parseBooleanOption(config, key)
  def parseShort(key: String): ParseResult[Short] = HoconUtils.parseShort(config, key)
  def parseShortOption(key: String): ParseResult[Option[Short]] = HoconUtils.parseShortOption(config, key)
  def parseInt(key: String): ParseResult[Int] = HoconUtils.parseInt(config, key)
  def parseIntOption(key: String): ParseResult[Option[Int]] = HoconUtils.parseIntOption(config, key)
  def parseLong(key: String): ParseResult[Long] = HoconUtils.parseLong(config, key)
  def parseLongOption(key: String): ParseResult[Option[Long]] = HoconUtils.parseLongOption(config, key)
  def parseFloat(key: String): ParseResult[Float] = HoconUtils.parseFloat(config, key)
  def parseFloatOption(key: String): ParseResult[Option[Float]] = HoconUtils.parseFloatOption(config, key)
  def parseDouble(key: String): ParseResult[Double] = HoconUtils.parseDouble(config, key)
  def parseDoubleOption(key: String): ParseResult[Option[Double]] = HoconUtils.parseDoubleOption(config, key)
  def parseBigDecimal(key: String): ParseResult[BigDecimal] = HoconUtils.parseBigDecimal(config, key)
  def parseBigDecimalOption(key: String): ParseResult[Option[BigDecimal]] = HoconUtils.parseBigDecimalOption(config, key)
  def parseString(key: String): ParseResult[String] = HoconUtils.parseString(config, key)
  def parseStringOption(key: String): ParseResult[Option[String]] = HoconUtils.parseStringOption(config, key)
  def parseStringList(key: String): ParseResult[List[String]] = HoconUtils.parseStringList(config, key)
  def parseStringOrConfig(key: String): ParseResult[Either[String, Config]] = HoconUtils.parseStringOrConfig(config, key)
  def parseStringOrConfigOption(key: String): ParseResult[Option[Either[String, Config]]] = HoconUtils.parseStringOrConfigOption(config, key)
  def parseConfig(key: String): ParseResult[Config] = HoconUtils.parseConfig(config, key)
  def parseConfigList(key: String): ParseResult[List[Config]] = HoconUtils.parseConfigList(config, key)
  def parseConfigOrConfigList(key: String): ParseResult[Either[Config, List[Config]]] = HoconUtils.parseConfigOrConfigList(config, key)
  def parseAsConfigList(key: String): ParseResult[List[Config]] = HoconUtils.parseAsConfigList(config, key)
  def parseObjectList[T](key: String, f: Config => ParseResult[T]): ParseResult[List[T]] = HoconUtils.parseObjectList(config, key, f)
  def parseAsObjectList[T](key: String, f: Config => ParseResult[T]): ParseResult[List[T]] = HoconUtils.parseAsObjectList(config, key, f)

  def cString(key: String): Consequence[String] = HoconUtils.consequenceString(config, key)
  def cStringOption(key: String): Consequence[Option[String]] = HoconUtils.consequenceStringOption(config, key)
  def cBigDecimal(key: String): Consequence[BigDecimal] = HoconUtils.consequenceBigDecimal(config, key)
  def cRational(key: String): Consequence[Rational] = HoconUtils.consequenceRational(config, key)
  def cRationalOption(key: String): Consequence[Option[Rational]] = HoconUtils.consequenceRationalOption(config, key)
  def cAsConfig(key: String): Consequence[Config] = HoconUtils.consequenceAsConfig(config, key)
  def cAsConfigList(key: String): Consequence[List[Config]] = HoconUtils.consequenceAsConfigList(config, key)
  def cEagerStringList(key: String): Consequence[List[String]] = Consequence(HoconUtils.getEagerStringList(config, key) getOrElse Nil)

  def consequenceBoolean(key: String): Consequence[Boolean] = HoconUtils.consequenceBoolean(config, key)
  def consequenceBoolean(key: String, default: Boolean): Consequence[Boolean] = HoconUtils.consequenceBoolean(config, key, default)
  def consequenceBooleanOption(key: String): Consequence[Option[Boolean]] = HoconUtils.consequenceBooleanOption(config, key)
  def consequenceShort(key: String): Consequence[Short] = HoconUtils.consequenceShort(config, key)
  def consequenceShortOption(key: String): Consequence[Option[Short]] = HoconUtils.consequenceShortOption(config, key)
  def consequenceInt(key: String): Consequence[Int] = HoconUtils.consequenceInt(config, key)
  def consequenceIntOption(key: String): Consequence[Option[Int]] = HoconUtils.consequenceIntOption(config, key)
  def consequenceLong(key: String): Consequence[Long] = HoconUtils.consequenceLong(config, key)
  def consequenceLongOption(key: String): Consequence[Option[Long]] = HoconUtils.consequenceLongOption(config, key)
  def consequenceFloat(key: String): Consequence[Float] = HoconUtils.consequenceFloat(config, key)
  def consequenceFloatOption(key: String): Consequence[Option[Float]] = HoconUtils.consequenceFloatOption(config, key)
  def consequenceDouble(key: String): Consequence[Double] = HoconUtils.consequenceDouble(config, key)
  def consequenceDoubleOption(key: String): Consequence[Option[Double]] = HoconUtils.consequenceDoubleOption(config, key)
  def consequenceBigDecimal(key: String): Consequence[BigDecimal] = HoconUtils.consequenceBigDecimal(config, key)
  def consequenceBigDecimalOption(key: String): Consequence[Option[BigDecimal]] = HoconUtils.consequenceBigDecimalOption(config, key)
  def consequenceString(key: String): Consequence[String] = HoconUtils.consequenceString(config, key)
  def consequenceStringOption(key: String): Consequence[Option[String]] = HoconUtils.consequenceStringOption(config, key)
  def consequenceUrl(key: String): Consequence[URL] = HoconUtils.consequenceUrl(config, key)
  def consequenceUrlOption(key: String): Consequence[Option[URL]] = HoconUtils.consequenceUrlOption(config, key)
  def consequenceDuration(key: String): Consequence[Duration] = HoconUtils.consequenceDuration(config, key)
  def consequenceDuration(key: String, d: Duration): Consequence[Duration] = HoconUtils.consequenceDuration(config, key, d)
  def consequenceDurationOption(key: String): Consequence[Option[Duration]] = HoconUtils.consequenceDurationOption(config, key)
  def consequenceDurationByMinute(key: String): Consequence[Duration] = HoconUtils.consequenceDurationByMinute(config, key)
  def consequenceDurationByMinute(key: String, d: Duration): Consequence[Duration] = HoconUtils.consequenceDurationByMinute(config, key, d)
  def consequenceDurationByMinuteOption(key: String): Consequence[Option[Duration]] = HoconUtils.consequenceDurationByMinuteOption(config, key)
  def consequenceDurationByHour(key: String): Consequence[Duration] = HoconUtils.consequenceDurationByHour(config, key)
  def consequenceDurationByHour(key: String, d: Duration): Consequence[Duration] = HoconUtils.consequenceDurationByHour(config, key, d)
  def consequenceDurationByHourOption(key: String): Consequence[Option[Duration]] = HoconUtils.consequenceDurationByHourOption(config, key)
  def consequenceDurationByDay(key: String): Consequence[Duration] = HoconUtils.consequenceDurationByDay(config, key)
  def consequenceDurationByDay(key: String, d: Duration): Consequence[Duration] = HoconUtils.consequenceDurationByDay(config, key, d)
  def consequenceDurationByDayOption(key: String): Consequence[Option[Duration]] = HoconUtils.consequenceDurationByDayOption(config, key)
  def consequenceFiniteDuration(key: String): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDuration(config, key)
  def consequenceFiniteDuration(key: String, d: FiniteDuration): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDuration(config, key, d)
  def consequenceFiniteDurationOption(key: String): Consequence[Option[FiniteDuration]] = HoconUtils.consequenceFiniteDurationOption(config, key)
  def consequenceFiniteDurationByMinute(key: String): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByMinute(config, key)
  def consequenceFiniteDurationByMinute(key: String, d: FiniteDuration): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByMinute(config, key, d)
  def consequenceFiniteDurationByMinuteOption(key: String): Consequence[Option[FiniteDuration]] = HoconUtils.consequenceFiniteDurationByMinuteOption(config, key)
  def consequenceFiniteDurationByHour(key: String): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByHour(config, key)
  def consequenceFiniteDurationByHour(key: String, d: FiniteDuration): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByHour(config, key, d)
  def consequenceFiniteDurationByHourOption(key: String): Consequence[Option[FiniteDuration]] = HoconUtils.consequenceFiniteDurationByHourOption(config, key)
  def consequenceFiniteDurationByDay(key: String): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByDay(config, key)
  def consequenceFiniteDurationByDay(key: String, default: FiniteDuration): Consequence[FiniteDuration] = HoconUtils.consequenceFiniteDurationByDay(config, key, default)
  def consequenceFiniteDurationByDayOption(key: String): Consequence[Option[FiniteDuration]] = HoconUtils.consequenceFiniteDurationByDayOption(config, key)
  def consequenceDateTime(key: String): Consequence[DateTime] = HoconUtils.consequenceDateTime(config, key)
  def consequenceDateTimeOption(key: String): Consequence[Option[DateTime]] = HoconUtils.consequenceDateTimeOption(config, key)
  def consequenceDateTimeWithContext(key: String)(implicit ctx: DateTimeContext): Consequence[DateTime] = HoconUtils.consequenceDateTimeWithContext(config, key)
  def consequenceDateTimeOptionWithContext(key: String)(implicit ctx: DateTimeContext): Consequence[Option[DateTime]] = HoconUtils.consequenceDateTimeOptionWithContext(config, key)

  def consequenceToken[T <: ValueInstance](key: String, f: ValueClass[T]): Consequence[T] =
    HoconUtils.consequenceToken(config, key, f)
  def consequenceTokenOption[T <: ValueInstance](key: String, f: ValueClass[T]): Consequence[Option[T]] =
    HoconUtils.consequenceTokenOption(config, key, f)

  def consequenceStringOrConfig(key: String): Consequence[Either[String, Config]] = HoconUtils.consequenceStringOrConfig(config, key)
  def consequenceStringOrConfigOption(key: String): Consequence[Option[Either[String, Config]]] = HoconUtils.consequenceStringOrConfigOption(config, key)
  def consequenceConfig(key: String): Consequence[Config] = HoconUtils.consequenceConfig(config, key)
  def consequenceConfigOption(key: String): Consequence[Option[Config]] = HoconUtils.consequenceConfigOption(config, key)
  def consequenceConfigList(key: String): Consequence[List[Config]] = HoconUtils.consequenceConfigList(config, key)
  def consequenceConfigOrConfigList(key: String): Consequence[Either[Config, List[Config]]] = HoconUtils.consequenceConfigOrConfigList(config, key)
  def consequenceAsConfigList(key: String): Consequence[List[Config]] = HoconUtils.consequenceAsConfigList(config, key)
  def consequenceObjectList[T](key: String, f: Config => Consequence[T]): Consequence[List[T]] = HoconUtils.consequenceObjectList(config, key, f)
  def consequenceAsObjectList[T](key: String, f: Config => Consequence[T]): Consequence[List[T]] = HoconUtils.consequenceAsObjectList(config, key, f)

  def childConfigMap: Map[String, Config] = HoconUtils.childConfigMap(config)
  def childRichConfigMap: Map[String, RichConfig] = HoconUtils.childRichConfigMap(config)
  def buildMap[T](f: Config => Option[T]): VectorMap[String, T] = HoconUtils.buildMap(config, f)
  def buildMap[T](key: String, f: Config => Option[T]): VectorMap[String, T] = HoconUtils.buildMap(config, f, key)

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

  sealed trait StringOrConfigOrConfigList
  object StringOrConfigOrConfigList {
    case class S(s: String) extends StringOrConfigOrConfigList
    case class C(c: Config) extends StringOrConfigOrConfigList
    case class CL(cl: List[Config]) extends StringOrConfigOrConfigList
  }
}
