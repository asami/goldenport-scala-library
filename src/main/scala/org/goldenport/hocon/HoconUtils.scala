package org.goldenport.hocon

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import java.util.Locale
import java.net.{URL, URI}
import java.io.File
import org.joda.time.DateTime
import org.joda.time.LocalDateTime
import org.joda.time.LocalDate
import org.joda.time.LocalTime
import spire.math.Rational
import com.typesafe.config._
import com.asamioffice.goldenport.io.UURL
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.value._
import org.goldenport.collection.VectorMap
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.extension.IRecord
import org.goldenport.parser.ParseResult
import org.goldenport.util.TypesafeConfigUtils
import org.goldenport.util.DateTimeUtils
import org.goldenport.util.LocalDateTimeUtils
import org.goldenport.util.LocalDateUtils
import org.goldenport.util.LocalTimeUtils
import org.goldenport.util.AnyUtils
import org.goldenport.hocon.RichConfig.StringOrConfigOrConfigList

/*
 * Migrated from org.goldenport.util.HoconUtils.
 * 
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
 *  version Dec. 22, 2019
 *  version Jun. 18, 2020
 *  version Apr. 30, 2021
 *  version May.  5, 2021
 *  version Jun. 13, 2021
 *  version Oct. 20, 2021
 *  version Dec. 31, 2021
 *  version Jan. 27, 2022
 *  version Feb. 17, 2022
 *  version Mar. 11, 2022
 *  version Oct. 13, 2022
 *  version Nov. 29, 2022
 *  version Dec. 12, 2022
 *  version Apr. 10, 2023
 *  version Nov. 22, 2023
 * @version Apr.  6, 2025
 * @author  ASAMI, Tomoharu
 */
object HoconUtils {
  val empty = ConfigFactory.empty()

  def parse(p: String): RichConfig = new RichConfig(ConfigFactory.parseString(p))

  def parseConfig(p: String): Consequence[Config] = Consequence run (
    try {
      val r = ConfigFactory.parseString(p)
      Consequence.success(r)
    } catch {
      case m: com.typesafe.config.ConfigException => Consequence.syntaxErrorFault(Option(m.getMessage) getOrElse p)
    }
  )

  def isDefined(config: Config, key: String): Boolean = config.hasPath(key)

  def asBoolean(config: Config, key: String, fallback: Boolean): Boolean =
    if (config.hasPath(key))
      config.getBoolean(key)
    else
      fallback

  def asConfig(config: Config, key: String): Config =
    getConfig(config, key) getOrElse empty

  def asConfigList(config: Config, key: String): List[Config] =
    getConfigOrConfigList(config, key) match {
      case Some(s) => s match {
        case Right(r) => r
        case Left(l) => List(l)
      }
      case None => Nil
    }

  def asStringList(config: Config, key: String): List[String] =
    getStringList(config, key) getOrElse Nil

  def asEagerStringList(config: Config, key: String): List[String] =
    getEagerStringList(config , key) getOrElse Nil

  def asStringVector(config: Config, key: String): Vector[String] =
    asStringList(config, key).toVector

  def asEagerStringVector(config: Config, key: String): Vector[String] =
    asStringList(config, key).toVector

  def asUrlList(config: Config, key: String): List[URL] =
    asStringList(config, key).map(new URL(_))

  def asDuration(config: Config, key: String, fallback: FiniteDuration): Duration =
    getDuration(config, key) getOrElse fallback

  def takeString(config: Config, key: String): String = config.getString(key)

  def takeUrl(config: Config, key: String): URL =
    new URL(config.getString(key))

  def takeUri(config: Config, key: String): URI =
    new URI(config.getString(key))

  def takeLocale(config: Config, key: String): Locale = new Locale(config.getString(key))

  def takeI18NString(config: Config, key: String): I18NString =
    I18NString.parse(config.getString(key))

  def takeI18NElement(config: Config, key: String): I18NElement =
    I18NElement.parse(config.getString(key))

  def takeConfig(config: Config, key: String): Config =
    ParseResult.orMissing(key, Option(config.getConfig(key))).take

  def takeConfigList(config: Config, key: String): List[Config] =
    Option(config.getConfigList(key)).map(_.asScala.toList) getOrElse Nil

  def takeConfigOrConfigList(config: Config, key: String): Either[Config, List[Config]] =
    getConfigOrConfigList(config, key).get

  def takeValue[T <: ValueInstance](valueclass: ValueClass[T], config: Config, key: String): T =
    getString(config, key).
      map(x => valueclass.get(x).
        getOrElse(RAISE.invalidArgumentFault(s"Invalid value name: $key"))).
      getOrElse(RAISE.invalidArgumentFault(s"Not found: $key"))

  def takeValue[T <: ValueInstance](valueclass: ValueClass[T], config: Config, key: String, default: T): T =
    getString(config, key).
      map(x => valueclass.get(x).
        getOrElse(RAISE.invalidArgumentFault(s"Invalid value name: $key"))).
      getOrElse(default)

  def getObject(config: Config, key: String): Option[ConfigObject] =
    if (config.hasPath(key))
      Some(config.getObject(key))
    else
      None

  def getBoolean(config: Config, key: String): Option[Boolean] =
    if (config.hasPath(key))
      Some(config.getBoolean(key))
    else
      None

  def getShort(config: Config, key: String): Option[Short] =
    if (config.hasPath(key))
      Some(config.getInt(key)).map(_.toShort)
    else
      None

  def getInt(config: Config, key: String): Option[Int] =
    if (config.hasPath(key))
      Some(config.getInt(key))
    else
      None

  def getLong(config: Config, key: String): Option[Long] =
    if (config.hasPath(key))
      Some(config.getLong(key))
    else
      None

  def getFloat(config: Config, key: String): Option[Float] =
    if (config.hasPath(key))
      Some(config.getDouble(key)).map(_.toFloat)
    else
      None

  def getDouble(config: Config, key: String): Option[Double] =
    if (config.hasPath(key))
      Some(config.getDouble(key))
    else
      None

  def getBigDecimal(config: Config, key: String): Option[BigDecimal] =
    if (config.hasPath(key))
      Some(config.getString(key)).map(BigDecimal(_))
    else
      None

  def getRational(config: Config, key: String): Option[Rational] =
    if (config.hasPath(key))
      Some(config.getString(key)).map(Rational(_))
    else
      None

  def getString(config: Config, key: String): Option[String] =
    if (config.hasPath(key))
      Some(config.getString(key))
    else
      None

  def getDuration(config: Config, key: String): Option[Duration] =
    consequenceDurationOption(config, key).take

  def getDurationByMinute(config: Config, key: String): Option[Duration] =
    consequenceDurationByMinuteOption(config, key).take

  def getFiniteDuration(config: Config, key: String): Option[FiniteDuration] =
    consequenceFiniteDurationOption(config, key).take

  def getFiniteDurationByMinute(config: Config, key: String): Option[FiniteDuration] =
    consequenceFiniteDurationByMinuteOption(config, key).take

  def getConfigList(config: Config, key: String): Option[List[Config]] =
    if (config.hasPath(key))
      Option(config.getConfigList(key).asScala.toList)
    else
      None

  def getStringList(config: Config, key: String): Option[List[String]] =
    if (config.hasPath(key))
      Some(_get_string_list(config, key))
    else
      None

  private def _get_string_list(config: Config, key: String): List[String] = try {
    config.getStringList(key).asScala.toList
  } catch {
    case NonFatal(e) => Strings.totokens(config.getString(key))
  }

  def getEagerStringList(config: Config, key: String): Option[List[String]] =
    if (config.hasPath(key))
      Some(try {
        config.getStringList(key).asScala.toList.flatMap(Strings.totokens)
      } catch {
        case NonFatal(e) => Strings.totokens(config.getString(key))
      })
    else
      None

  def getNonEmptyListString(config: Config, key: String): Option[NonEmptyList[String]] =
    asStringList(config, key) match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, xs))
    }

  def getEagerNonEmptyListString(config: Config, key: String): Option[NonEmptyList[String]] =
    asEagerStringList(config, key) match {
      case Nil => None
      case x :: xs => Some(NonEmptyList.nel(x, xs))
    }

  def getUrl(config: Config, key: String): Option[URL] =
    if (config.hasPath(key))
      Some(takeUrl(config, key))
    else
      None

  def getUri(config: Config, key: String): Option[URI] =
    if (config.hasPath(key))
      Some(takeUri(config, key))
    else
      None

  def getUriList(config: Config, key: String): Option[List[URI]] =
    getStringList(config, key).map(_.map(new URI(_)))

  def getLocale(config: Config, key: String): Option[Locale] =
    if (config.hasPath(key))
      Some(takeLocale(config, key))
    else
      None

  def getI18NString(config: Config, key: String): Option[I18NString] =
    if (config.hasPath(key))
      Some(takeI18NString(config, key))
    else
      None

  def getI18NElement(config: Config, key: String): Option[I18NElement] =
    if (config.hasPath(key))
      Some(takeI18NElement(config, key))
    else
      None

  def getConfig(config: Config, key: String): Option[Config] =
    if (config.hasPath(key))
      Option(config.getConfig(key))
    else
      None

  def getRichConfig(config: Config, key: String): Option[RichConfig] =
    getConfig(config, key).map(RichConfig.apply)

  def getConfigOrConfigList(config: Config, key: String): Option[Either[Config, List[Config]]] =
    if (config.hasPath(key))
      config.getValue(key) match {
        case m: ConfigObject => Some(Left(m.toConfig))
        case m: ConfigList => Some(Right(m.asScala.toList.map(_to_config)))
      }
    else
      None

  private def _to_config(p: ConfigValue): Config = p match {
    case m: ConfigObject => m.toConfig
  }

  def getValue[T <: ValueInstance](valueclass: ValueClass[T], config: Config, key: String): Option[T] =
    getString(config, key).
      map(x => valueclass.get(x).
        getOrElse(RAISE.invalidArgumentFault(s"Invalid value name: $key")))

  def getFile(config: Config, key: String): Option[File] =
    getString(config, key).map(new File(_))

  def toFlattenList(p: Config): List[(String, Any)] =
    p.entrySet().asScala.toList.map(x => x.getKey -> x.getValue.unwrapped)

  def toFlattenVector(p: Config): Vector[(String, Any)] =
    p.entrySet().asScala.toVector.map(x => x.getKey -> x.getValue.unwrapped)

  def toFlattenMap(p: Config): Map[String, Any] = toFlattenVector(p).toMap

  def toFlattenVectorMap(p: Config): VectorMap[String, Any] =
    VectorMap(toFlattenVector(p))

  //
  def getObject(p: ConfigValue): Option[ConfigObject] = Option(p) collect {
    case m: ConfigObject => m
  }

  def getObject(p: ConfigObject, key: String): Option[ConfigObject] =
    Option(p.get(key)).flatMap(getObject)

  def getValue(p: ConfigValue, key: String): Option[ConfigValue] =
    getObject(p).flatMap(x => Option(x.get(key)))

  def getString(p: ConfigValue, key: String): Option[String] = 
    getValue(p, key).map(x => AnyUtils.toString(x.unwrapped))

  def getInt(p: ConfigValue, key: String): Option[Int] = 
    getValue(p, key).map(x => AnyUtils.toInt(x.unwrapped))

  //
  def parseBoolean(p: Config, key: String): ParseResult[Boolean] =
    getBoolean(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseBooleanOption(p: Config, key: String): ParseResult[Option[Boolean]] = ParseResult(
    getBoolean(p, key)
  )

  def parseShort(p: Config, key: String): ParseResult[Short] =
    getShort(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseShortOption(p: Config, key: String): ParseResult[Option[Short]] = ParseResult(
    getShort(p, key)
  )

  def parseInt(p: Config, key: String): ParseResult[Int] =
    getInt(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseIntOption(p: Config, key: String): ParseResult[Option[Int]] = ParseResult(
    getInt(p, key)
  )

  def parseLong(p: Config, key: String): ParseResult[Long] =
    getLong(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseLongOption(p: Config, key: String): ParseResult[Option[Long]] = ParseResult(
    getLong(p, key)
  )

  def parseFloat(p: Config, key: String): ParseResult[Float] =
    getFloat(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseFloatOption(p: Config, key: String): ParseResult[Option[Float]] = ParseResult(
    getFloat(p, key)
  )

  def parseDouble(p: Config, key: String): ParseResult[Double] =
    getDouble(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseDoubleOption(p: Config, key: String): ParseResult[Option[Double]] = ParseResult(
    getDouble(p, key)
  )

  def parseBigDecimal(p: Config, key: String): ParseResult[BigDecimal] =
    getBigDecimal(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseBigDecimalOption(p: Config, key: String): ParseResult[Option[BigDecimal]] = ParseResult(
    getBigDecimal(p, key)
  )

  def parseString(p: Config, key: String): ParseResult[String] =
    getString(p, key) match {
      case Some(s) => ParseResult.success(s)
      case None => ParseResult.missing(key)
    }

  def parseStringOption(p: Config, key: String): ParseResult[Option[String]] = ParseResult(
    getString(p, key)
  )

  def parseStringList(p: Config, key: String): ParseResult[List[String]] = ParseResult(
    asStringList(p, key)
  )

  def parseStringOrConfig(p: Config, key: String): ParseResult[Either[String, Config]] =
    if (p.hasPath(key)) {
      val v = p.getValue(key)
      v match {
        case m: ConfigObject => ParseResult(Right(m.toConfig))
        case m => m.unwrapped match {
          case null => ParseResult.error(s"Missiong $key")
          case m: String => ParseResult.success(Left(m))
          case m => ParseResult.error(s"Not string or object: $m")
        }
      }
    } else {
     ParseResult.error(s"Missiong $key")
    }

  def parseStringOrConfigOption(p: Config, key: String): ParseResult[Option[Either[String, Config]]] =
    if (p.hasPath(key)) {
      val v = p.getValue(key)
      v match {
        case m: ConfigObject => ParseResult(Some(Right(m.toConfig)))
        case m => m.unwrapped match {
          case null => ParseResult.success(None)
          case m: String => ParseResult.success(Some(Left(m)))
          case m => ParseResult.error(s"Not string or object: $m")
        }
      }
    } else {
      ParseResult.success(None)
    }

  def parseConfig(p: Config, key: String): ParseResult[Config] =
    ParseResult.orMissing(key, getConfig(p, key))

  def parseConfigList(p: Config, key: String): ParseResult[List[Config]] =
    ParseResult(takeConfigList(p, key))

  def parseAsConfigList(p: Config, key: String): ParseResult[List[Config]] =
    ParseResult(asConfigList(p, key))

  def parseObjectList[T](p: Config, key: String, f: Config => ParseResult[T]): ParseResult[List[T]] =
    takeConfigList(p, key).traverse(f)

  def parseAsObjectList[T](p: Config, key: String, f: Config => ParseResult[T]): ParseResult[List[T]] =
    asConfigList(p, key).traverse(f)

  def parseConfigOrConfigList(p: Config, key: String): ParseResult[Either[Config, List[Config]]] =
    ParseResult(takeConfigOrConfigList(p, key))

  def consequenceBoolean(p: Config, key: String): Consequence[Boolean] =
    getBoolean(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceBoolean(p: Config, key: String, default: Boolean): Consequence[Boolean] =
    getBoolean(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceBooleanOption(p: Config, key: String): Consequence[Option[Boolean]] = Consequence(
    getBoolean(p, key)
  )

  def consequenceShort(p: Config, key: String): Consequence[Short] =
    getShort(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceShortOption(p: Config, key: String): Consequence[Option[Short]] = Consequence(
    getShort(p, key)
  )

  def consequenceInt(p: Config, key: String): Consequence[Int] =
    getInt(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceIntOption(p: Config, key: String): Consequence[Option[Int]] = Consequence(
    getInt(p, key)
  )

  def consequenceLong(p: Config, key: String): Consequence[Long] =
    getLong(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLongOption(p: Config, key: String): Consequence[Option[Long]] = Consequence(
    getLong(p, key)
  )

  def consequenceFloat(p: Config, key: String): Consequence[Float] =
    getFloat(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceFloatOption(p: Config, key: String): Consequence[Option[Float]] = Consequence(
    getFloat(p, key)
  )

  def consequenceDouble(p: Config, key: String): Consequence[Double] =
    getDouble(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceDoubleOption(p: Config, key: String): Consequence[Option[Double]] = Consequence(
    getDouble(p, key)
  )

  def consequenceBigDecimal(p: Config, key: String): Consequence[BigDecimal] =
    getBigDecimal(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceBigDecimalOption(p: Config, key: String): Consequence[Option[BigDecimal]] = Consequence(
    getBigDecimal(p, key)
  )

  def consequenceRational(p: Config, key: String): Consequence[Rational] =
    getRational(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceRationalOption(p: Config, key: String): Consequence[Option[Rational]] = Consequence(
    getRational(p, key)
  )

  def consequenceString(p: Config, key: String): Consequence[String] =
    getString(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceStringOption(p: Config, key: String): Consequence[Option[String]] = Consequence(
    getString(p, key)
  )

  def consequenceUrl(p: Config, key: String): Consequence[URL] =
    getString(p, key) match {
      case Some(s) => Consequence(UURL.getURLFromFileOrURLName(s))
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceUrlOption(p: Config, key: String): Consequence[Option[URL]] =
    consequenceStringOption(p, key).flatMap(x => Consequence(x.map(UURL.getURLFromFileOrURLName)))

  def consequenceMillisecondsOption(config: Config, key: String): Consequence[Option[Long]] =
    if (config.hasPath(key))
      Consequence(config.getDuration(key, MILLISECONDS)).map(Some(_))
    else
      Consequence(None)

  def consequenceSeconds(config: Config, key: String): Consequence[Option[Long]] =
    if (config.hasPath(key))
      Consequence(config.getDuration(key, SECONDS)).map(Some(_))
    else
      Consequence(None)

  def consequenceMinutesOption(config: Config, key: String): Consequence[Option[Long]] =
    if (config.hasPath(key))
      Consequence(config.getDuration(key, MINUTES)).map(Some(_))
    else
      Consequence(None)

  def consequenceHoursOption(config: Config, key: String): Consequence[Option[Long]] =
    if (config.hasPath(key))
      Consequence(config.getDuration(key, HOURS)).map(Some(_))
    else
      Consequence(None)

  def consequenceDaysOption(config: Config, key: String): Consequence[Option[Long]] =
    if (config.hasPath(key))
      Consequence(config.getDuration(key, DAYS)).map(Some(_))
    else
      Consequence(None)

  def consequenceDuration(p: Config, key: String): Consequence[Duration] =
    for {
      a <- consequenceDurationOption(p, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceDuration(p: Config, key: String, d: Duration): Consequence[Duration] =
    for {
      a <- consequenceDurationOption(p, key)
      r <- Consequence.success(a getOrElse d)
    } yield r

  def consequenceDurationOption(p: Config, key: String): Consequence[Option[Duration]] = {
    def getstring = AnyUtils.consequenceDuration(p.getString(key)).map(Option(_))
    def getlong = Consequence(p.getLong(key).milliseconds).map(Option(_))
    def getms = consequenceMillisecondsOption(p, key).map(_.map(_.milliseconds))
    if (p.hasPath(key))
      (getstring orElse getlong orElse getms)
    else
      Consequence.success(None)
  }

  def consequenceDurationByMinute(config: Config, key: String): Consequence[Duration] =
    for {
      a <- consequenceDurationByMinuteOption(config, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceDurationByMinute(config: Config, key: String, d: Duration): Consequence[Duration] =
    for {
      a <- consequenceDurationByMinuteOption(config, key)
      r <- Consequence.success(a getOrElse d)
    } yield r

  def consequenceDurationByMinuteOption(config: Config, key: String): Consequence[Option[Duration]] = {
    def getstring = AnyUtils.consequenceDuration(config.getString(key)).map(Option(_))
    def getlong = Consequence(config.getLong(key).minutes).map(Option(_))
    def getms = consequenceMinutesOption(config, key).map(_.map(_.minutes))
    if (config.hasPath(key))
      (getstring orElse getlong orElse getms)
    else
      Consequence.success(None)
  }

  def consequenceDurationByHour(config: Config, key: String): Consequence[Duration] =
    for {
      a <- consequenceDurationByHourOption(config, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceDurationByHour(config: Config, key: String, d: Duration): Consequence[Duration] =
    for {
      a <- consequenceDurationByHourOption(config, key)
      r <- Consequence.success(a getOrElse d)
    } yield r

  def consequenceDurationByHourOption(config: Config, key: String): Consequence[Option[Duration]] = {
    def getstring = AnyUtils.consequenceDuration(config.getString(key)).map(Option(_))
    def getlong = Consequence(config.getLong(key).seconds).map(Option(_))
    def getms = consequenceHoursOption(config, key).map(_.map(_.hours))
    if (config.hasPath(key))
      (getstring orElse getlong orElse getms)
    else
      Consequence.success(None)
  }

  def consequenceDurationByDay(config: Config, key: String): Consequence[Duration] =
    for {
      a <- consequenceDurationByDayOption(config, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceDurationByDay(config: Config, key: String, d: Duration): Consequence[Duration] =
    for {
      a <- consequenceDurationByDayOption(config, key)
      r <- Consequence.success(a getOrElse d)
    } yield r

  def consequenceDurationByDayOption(config: Config, key: String): Consequence[Option[Duration]] = {
    def getstring = AnyUtils.consequenceDuration(config.getString(key)).map(Option(_))
    def getlong = Consequence(config.getLong(key).seconds).map(Option(_))
    def getms = consequenceDaysOption(config, key).map(_.map(_.days))
    if (config.hasPath(key))
      (getstring orElse getlong orElse getms)
    else
      Consequence.success(None)
  }

  def consequenceFiniteDuration(p: Config, key: String): Consequence[FiniteDuration] =
    for {
      x <- consequenceFiniteDurationOption(p, key)
      r <- Consequence.successOrMissingPropertyFault(key, x)
    } yield r

  def consequenceFiniteDuration(p: Config, key: String, d: FiniteDuration): Consequence[FiniteDuration] =
    consequenceFiniteDurationOption(p, key).map(_.getOrElse(d))

  def consequenceFiniteDurationOption(p: Config, key: String): Consequence[Option[FiniteDuration]] =
    for {
      a <- consequenceDurationOption(p, key)
      r <- _option_finitduration(key, a)
    } yield r

  def consequenceFiniteDurationByMinute(p: Config, key: String): Consequence[FiniteDuration] =
    for {
      a <- consequenceFiniteDurationByMinuteOption(p, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceFiniteDurationByMinute(p: Config, key: String, d: FiniteDuration): Consequence[FiniteDuration] =
    consequenceFiniteDurationByMinuteOption(p, key).map(_ getOrElse d)

  def consequenceFiniteDurationByMinuteOption(p: Config, key: String): Consequence[Option[FiniteDuration]] =
    for {
      a <- consequenceDurationByMinuteOption(p, key)
      r <- _option_finitduration(key, a)
    } yield r

  private def _option_finitduration(key: String, p: Option[Duration]) = p match {
    case Some(s) => s match {
      case m: FiniteDuration => Consequence.success(Some(m))
      case m => Consequence.invalidArgumentFault(key, AnyUtils.toString(m))
    }
    case None => Consequence.success(None)
  }

  def consequenceFiniteDurationByHour(p: Config, key: String): Consequence[FiniteDuration] =
    for {
      a <- consequenceFiniteDurationByHourOption(p, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceFiniteDurationByHour(p: Config, key: String, d: FiniteDuration): Consequence[FiniteDuration] =
    consequenceFiniteDurationByHourOption(p, key).map(_ getOrElse d)

  def consequenceFiniteDurationByHourOption(p: Config, key: String): Consequence[Option[FiniteDuration]] =
    for {
      a <- consequenceDurationByHourOption(p, key)
      r <- _option_finitduration(key, a)
    } yield r

  def consequenceFiniteDurationByDay(p: Config, key: String): Consequence[FiniteDuration] =
    for {
      a <- consequenceFiniteDurationByDayOption(p, key)
      r <- Consequence.successOrMissingPropertyFault(key, a)
    } yield r

  def consequenceFiniteDurationByDay(p: Config, key: String, d: FiniteDuration): Consequence[FiniteDuration] =
    for {
      a <- consequenceFiniteDurationByDayOption(p, key)
      r <- Consequence.success(a getOrElse d)
    } yield r

  def consequenceFiniteDurationByDayOption(p: Config, key: String): Consequence[Option[FiniteDuration]] =
    for {
      a <- consequenceDurationByDayOption(p, key)
      r <- _option_finitduration(key, a)
    } yield r

  def consequenceDateTimeWithContext(p: Config, key: String)(implicit ctx: DateTimeContext): Consequence[DateTime] =
    getString(p, key) match {
      case Some(s) => DateTimeUtils.consequenceDateTimeWithContext(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceDateTimeOptionWithContext(p: Config, key: String)(implicit ctx: DateTimeContext): Consequence[Option[DateTime]] =
    for {
      s <- consequenceStringOption(p, key)
      x <- s.traverse(DateTimeUtils.consequenceDateTimeWithContext)
    } yield x

  def consequenceDateTime(p: Config, key: String): Consequence[DateTime] =
    getString(p, key) match {
      case Some(s) => DateTimeUtils.consequenceDateTime(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceDateTimeOption(p: Config, key: String): Consequence[Option[DateTime]] =
    for {
      s <- consequenceStringOption(p, key)
      x <- s.traverse(DateTimeUtils.consequenceDateTime)
    } yield x

  def consequenceLocalDateTime(p: Config, key: String): Consequence[LocalDateTime] =
    getString(p, key) match {
      case Some(s) => LocalDateTimeUtils.consequenceLocalDateTime(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLocalDateTimeOption(p: Config, key: String): Consequence[Option[LocalDateTime]] =
    for {
      s <- consequenceStringOption(p, key)
      x <- s.traverse(LocalDateTimeUtils.consequenceLocalDateTime)
    } yield x

  def consequenceLocalDate(p: Config, key: String): Consequence[LocalDate] =
    getString(p, key) match {
      case Some(s) => LocalDateUtils.consequenceLocalDate(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLocalDateOption(p: Config, key: String): Consequence[Option[LocalDate]] =
    for {
      s <- consequenceStringOption(p, key)
      x <- s.traverse(LocalDateUtils.consequenceLocalDate)
    } yield x

  def consequenceLocalTime(p: Config, key: String): Consequence[LocalTime] =
    getString(p, key) match {
      case Some(s) => LocalTimeUtils.consequenceLocalTime(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLocalTimeOption(p: Config, key: String): Consequence[Option[LocalTime]] =
    for {
      s <- consequenceStringOption(p, key)
      x <- s.traverse(LocalTimeUtils.consequenceLocalTime)
    } yield x

  def consequenceToken[T <: ValueInstance](p: Config, key: String, f: ValueClass[T]): Consequence[T] =
    getString(p, key) match {
      case Some(s) => Consequence.successOrMissingPropertyFault(key, f.get(s))
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceTokenOption[T <: ValueInstance](p: Config, key: String, f: ValueClass[T]): Consequence[Option[T]] =
    getString(p, key) match {
      case Some(s) => f.get(s) match {
        case Some(v) => Consequence.success(Some(v))
        case None => Consequence.invalidTokenFault(key, s)
      }
      case None => Consequence.success(None)
    }

  def consequenceStringOrConfig(p: Config, key: String): Consequence[Either[String, Config]] =
    if (p.hasPath(key)) {
      val v = p.getValue(key)
      v match {
        case m: ConfigObject => Consequence(Right(m.toConfig))
        case m => m.unwrapped match {
          case null => Consequence.missingPropertyFault(key)
          case m: String => Consequence.success(Left(m))
          case m => Consequence.invalidArgumentFault(s"Not string or object: $m")
        }
      }
    } else {
     Consequence.missingPropertyFault(key)
    }

  def consequenceStringOrConfigOption(p: Config, key: String): Consequence[Option[Either[String, Config]]] =
    if (p.hasPath(key)) {
      val v = p.getValue(key)
      v match {
        case m: ConfigObject => Consequence(Some(Right(m.toConfig)))
        case m => m.unwrapped match {
          case null => Consequence.success(None)
          case m: String => Consequence.success(Some(Left(m)))
          case m => Consequence.invalidArgumentFault(s"Not string or object: $m")
        }
      }
    } else {
      Consequence.success(None)
    }

  def consequenceStringOrConfigOrConfigListOption(p: Config, key: String): Consequence[Option[StringOrConfigOrConfigList]] =
    if (p.hasPath(key)) {
      val v = p.getValue(key)
      v match {
        case m: ConfigObject => Consequence(Some(StringOrConfigOrConfigList.C(m.toConfig)))
        case m: ConfigList => Consequence(Some(StringOrConfigOrConfigList.CL(m.asScala.toList.map(_to_config))))
        case m => m.unwrapped match {
          case null => Consequence.success(None)
          case m: String => Consequence.success(Some(StringOrConfigOrConfigList.S(m)))
          case m => Consequence.invalidArgumentFault(s"Not string or object: $m")
        }
      }
    } else {
      Consequence.success(None)
    }

  def consequenceConfig(p: Config, key: String): Consequence[Config] =
    Consequence.successOrMissingPropertyFault(key, getConfig(p, key))

  def consequenceConfigOption(p: Config, key: String): Consequence[Option[Config]] =
    Consequence(getConfig(p, key))

  def consequenceConfigList(p: Config, key: String): Consequence[List[Config]] =
    Consequence(takeConfigList(p, key))

  def consequenceAsConfig(p: Config, key: String): Consequence[Config] =
    Consequence(getConfig(p, key).getOrElse(empty))

  def consequenceAsConfigList(p: Config, key: String): Consequence[List[Config]] =
    Consequence(asConfigList(p, key))

  def consequenceObjectList[T](p: Config, key: String, f: Config => Consequence[T]): Consequence[List[T]] =
    takeConfigList(p, key).traverse(f)

  def consequenceAsObjectList[T](p: Config, key: String, f: Config => Consequence[T]): Consequence[List[T]] =
    asConfigList(p, key).traverse(f)

  def consequenceConfigOrConfigList(p: Config, key: String): Consequence[Either[Config, List[Config]]] =
    Consequence(takeConfigOrConfigList(p, key))

  // def childConfigSet(p: Config): Set[(String, Config)] = p.entrySet.asScala.
  //   flatMap { x =>
  //     x.getValue match {
  //       case m: Config => Some(x.getKey -> m)
  //       case m => None
  //     }
  //   }

  // def childRichConfigSet(p: Config): Set[(String, RichConfig)] =
  //   childConfigSet(p).mapValues(RichConfig.apply)

  // CAUTION: not work -> probably work
  def childConfigMap(p: Config): Map[String, Config] = {
    val a: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = p.root().entrySet.asScala
    a.flatMap { x =>
      x.getValue match {
        case m: ConfigObject => Some(x.getKey -> m.toConfig)
        case m: Config => Some(x.getKey -> m)
        case m => None
      }
    }.toMap
  }

  // CAUTION: not work -> probably work
  def childRichConfigMap(p: Config): Map[String, RichConfig] =
    childConfigMap(p).mapValues(RichConfig.apply)

  def buildMap[T](p: Config, f: Config => Option[T], key: String): VectorMap[String, T] =
    HoconUtils.getConfig(p, key).map(buildMap(_, f)).getOrElse(VectorMap.empty)

  def buildMap[T](p: Config, f: Config => Option[T]): VectorMap[String, T] =
    VectorMapBuilder(f).build(p)

  case class VectorMapBuilder[T](f: Config => Option[T]) {
    def build(p: Config): VectorMap[String, T] = {
      val a: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = p.root().entrySet.asScala
      val b = a.flatMap { x =>
        val y = x.getValue match {
          case m: ConfigObject => f(m.toConfig)
          case m: Config => f(m)
          case m => None
        }
        y.map(z => x.getKey -> z)
      }
      VectorMap(b.toVector)
    }
  }

  def makeJsonString(props: (String, Any)*): String = {
    val config = createHocon(props.toVector)
    config.root.render(ConfigRenderOptions.defaults.setJson(true).setComments(false).setOriginComments(false))
  }

  def makeJsonString(p: Map[String, Any]): String = {
    val config = createHocon(p)
    config.root.render(ConfigRenderOptions.defaults.setJson(true).setComments(false).setOriginComments(false))
  }

  def makeHoconString(props: (String, Any)*): String = {
    val config = createHocon(props.toVector)
    config.root.render(ConfigRenderOptions.defaults.setOriginComments(false))
  }

  def makeHoconString(p: Map[String, Any]): String = {
    val config = createHocon(p.toVector)
    config.root.render(ConfigRenderOptions.defaults.setOriginComments(false))
  }

  def buildHocon(props: (String, Any)*): Config = createHocon(props.toVector)

  def createHocon(props: Map[String, Any]): Config = createHocon(props.toVector)

  def createHocon(props: Seq[(String, Any)]): Config = {
    val config = ConfigFactory.empty
    props./:(config)((z, x) => z.withValue(x._1, _to_value(x._2)))
  }

  private def _to_value(p: Any): ConfigValue = p match {
    case m: String => ConfigValueFactory.fromAnyRef(m)
    case m: Rational => ConfigValueFactory.fromAnyRef(m.toString)
    case m: BigDecimal => ConfigValueFactory.fromAnyRef(m.toString)
    case m: Number => ConfigValueFactory.fromAnyRef(m)
    case m: IRecord => RAISE.notImplementedYetDefect
    case m: Map[_, _] => RAISE.notImplementedYetDefect
    case m => ConfigValueFactory.fromAnyRef(AnyUtils.toMarshall(m))
  }

  /*
   * Mutation
   */
  def addProperty(config: Config, key: String, value: Any): Config =
    config.withValue(key, ConfigValueFactory.fromAnyRef(value))
}
