package org.goldenport.util

import scalaz.{Success => _, Failure => _, _}
import scalaz.syntax.std.boolean._
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.io.File
import com.typesafe.config.{Config, ConfigFactory}
import org.goldenport.Strings
import com.asamioffice.goldenport.io.UURL

/*
 * See HoconUtils.
 * 
 * @since   Nov.  3, 2014
 *  version Dec. 19, 2015
 *  version Jun. 19, 2017
 *  version Jul. 30, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
object TypesafeConfigUtils {
  def isAvailable(key: String)(implicit config: Config): Boolean =
    config.hasPath(key) && Strings.notblankp(config.getString(key))

  def asString(key: String)(implicit config: Config): String = {
    val v = config.getString(key)
    assert (v != null, "%s should be not null".format(key))
    v
  }

  def asStringReader(key: String) =
    Reader((config: Config) => asString(key)(config))

  def getString(key: String)(implicit config: Config): Option[String] = {
    if (config.hasPath(key)) {
      val v = config.getString(key)
      assert (v != null, "%s should be not null".format(key))
      Some(v)
    } else {
//      log_debug("Config '%s' does not specified.".format(key))
      None
    }
  }

  def getNonBlankString(key: String)(implicit config: Config): Option[String] =
    if (config.hasPath(key)) {
      val v = config.getString(key)
      if (Strings.notblankp(v))
        Some(v)
      else
        None
    } else {
      None
    }

  def getStringReader(key: String) =
    Reader((config: Config) => getString(key)(config))

  def asLong(key: String, value: Long)(implicit config: Config): Long = {
    if (config.hasPath(key)) {
      Try(config.getLong(key)) match {
        case Success(s) => s
        case Failure(e) => {
//          log_warn(s"Config '$key' has illegal value: ${e.getMessage}")
          value
        }
      }
    } else {
//      log_debug("Config '%s' does not specified.".format(key))
      value
    }
  }

  def asDouble(key: String, value: Double)(implicit config: Config): Double = {
    if (config.hasPath(key)) {
      Try(config.getDouble(key)) match {
        case Success(s) => s
        case Failure(e) => {
//          log_warn(s"Config '$key' has illegal value: ${e.getMessage}")
          value
        }
      }
    } else {
//      log_debug("Config '%s' does not specified.".format(key))
      value
    }
  }

  def asMilliseconds(key: String, value: Long)(implicit config: Config): Long = {
    if (config.hasPath(key)) {
      Try(getMilliseconds(key).get) match {
        case Success(s) => s
        case Failure(e) => {
//          log_warn(s"Config '$key' has illegal value: ${e.getMessage}")
          value
        }
      }
    } else {
//      log_debug("Config '%s' does not specified.".format(key))
      value
    }
  }

  def getMilliseconds(key: String)(implicit config: Config): Option[Long] =
    config.hasPath(key) option config.getDuration(key, MILLISECONDS)

  def getDuration(key: String)(implicit config: Config): Option[FiniteDuration] = {
    def getlong: Try[Long] = Try(config.getLong(key).milliseconds.toMillis)
    def getms: Try[Long] = Try(getMilliseconds(key).get)
    if (config.hasPath(key)) {
      val ms = (getlong orElse getms).get
      Some(ms.milliseconds)
    } else {
      None
    }
  }

  def getDurationByMinute(key: String)(implicit config: Config): Option[FiniteDuration] = {
    def getlong: Try[Long] = Try(config.getLong(key).minutes.toMillis)
    def getms: Try[Long] = Try(getMilliseconds(key).get)
    if (config.hasPath(key)) {
      val ms = (getlong orElse getms).get
      Some(ms.milliseconds)
    } else {
      None
    }
  }

  def asDuration(key: String, value: FiniteDuration)(implicit config: Config): FiniteDuration = {
    def getlong: Try[Long] = Try(config.getLong(key) * 1000)
    def getms: Try[Long] = Try(getMilliseconds(key).get)
    if (config.hasPath(key)) {
      val ms = (getlong orElse getms).get
      ms.milliseconds
    } else {
      value
    }
  }

  def getFile(key: String)(implicit config: Config): Option[File] =
    getNonBlankString(key).map(UURL.getFileFromFileNameOrURLName)
}
