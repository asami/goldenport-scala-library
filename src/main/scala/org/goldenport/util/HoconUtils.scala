package org.goldenport.util

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import java.net.{URL, URI}
import com.typesafe.config.{ConfigFactory, Config}
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NElement}

/*
 * See TypesafeConfigUtils.
 *
 * @since   Nov. 17, 2016
 *  version Jun. 23, 2017
 *  version Aug. 29, 2017
 *  version Oct. 27, 2017
 *  version Dec. 14, 2017
 *  version Oct. 21, 2018
 * @version Nov.  5, 2018
 * @author  ASAMI, Tomoharu
 */
object HoconUtils {
  def parse(p: String): RichConfig = new RichConfig(ConfigFactory.parseString(p))

  def isDefined(config: Config, key: String): Boolean = config.hasPath(key)

  def asBoolean(config: Config, key: String, fallback: Boolean): Boolean =
    if (config.hasPath(key))
      config.getBoolean(key)
    else
      fallback

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

  def takeUrl(config: Config, key: String): URL =
    new URL(config.getString(key))

  def takeUri(config: Config, key: String): URI =
    new URI(config.getString(key))

  def takeI18NString(config: Config, key: String): I18NString =
    I18NString.parse(config.getString(key))

  def takeI18NElement(config: Config, key: String): I18NElement =
    I18NElement.parse(config.getString(key))

  def getBoolean(config: Config, key: String): Option[Boolean] =
    if (config.hasPath(key))
      Some(config.getBoolean(key))
    else
      None

  def getInt(config: Config, key: String): Option[Int] =
    if (config.hasPath(key))
      Some(config.getInt(key))
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

  def getConfig(config: Config, key: String): Option[RichConfig] =
    if (config.hasPath(key))
      Option(config.getConfig(key)).map(RichConfig.apply)
    else
      None

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
  }

  object Implicits {
    implicit def enrichHocon(config: Config) = new RichConfig(config)
  }
}
