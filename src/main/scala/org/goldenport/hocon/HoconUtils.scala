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
import com.typesafe.config._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NElement}
import org.goldenport.value._
import org.goldenport.collection.VectorMap
import org.goldenport.context.Consequence
import org.goldenport.parser.ParseResult
import org.goldenport.util.TypesafeConfigUtils
import org.goldenport.util.AnyUtils

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
 * @version Jan.  7, 2022
 * @author  ASAMI, Tomoharu
 */
object HoconUtils {
  val empty = ConfigFactory.empty()

  def parse(p: String): RichConfig = new RichConfig(ConfigFactory.parseString(p))

  def isDefined(config: Config, key: String): Boolean = config.hasPath(key)

  def asBoolean(config: Config, key: String, fallback: Boolean): Boolean =
    if (config.hasPath(key))
      config.getBoolean(key)
    else
      fallback

  def asConfig(config: Config, key: String): Config =
    getConfig(config, key) getOrElse empty

  def asConfigList(config: Config, key: String): List[Config] =
    getConfigList(config, key) getOrElse Nil

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

  def getString(config: Config, key: String): Option[String] =
    if (config.hasPath(key))
      Some(config.getString(key))
    else
      None

  def getDuration(config: Config, key: String): Option[FiniteDuration] =
    TypesafeConfigUtils.getDuration(key)(config)

  def getDurationByMinute(config: Config, key: String): Option[FiniteDuration] =
    TypesafeConfigUtils.getDurationByMinute(key)(config)

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

  def consequenceString(p: Config, key: String): Consequence[String] =
    getString(p, key) match {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceStringOption(p: Config, key: String): Consequence[Option[String]] = Consequence(
    getString(p, key)
  )

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

  def consequenceConfig(p: Config, key: String): Consequence[Config] =
    Consequence.successOrMissingPropertyFault(key, getConfig(p, key))

  def consequenceConfigList(p: Config, key: String): Consequence[List[Config]] =
    Consequence(takeConfigList(p, key))

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
}
