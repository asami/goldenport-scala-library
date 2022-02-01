package org.goldenport.context

import scalaz._, Scalaz._
import java.net.URL
import org.goldenport.value._
import org.goldenport.util.AnyUtils

/*
 * @since   Jan. 30, 2022
 * @version Jan. 30, 2022
 * @author  ASAMI, Tomoharu
 */
trait ConsequenceProperties {
  protected def get_Value(key: String): Option[Any]

  def consequenceBooleanOption(key: String): Consequence[Option[Boolean]] =
    get_Value(key).traverse(AnyUtils.consequenceBoolean)

  def consequenceBoolean(key: String): Consequence[Boolean] =
    consequenceBooleanOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceBoolean(key: String, default: Boolean): Consequence[Boolean] =
    consequenceBooleanOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceShortOption(key: String): Consequence[Option[Short]] =
    get_Value(key).traverse(AnyUtils.consequenceShort)

  def consequenceShort(key: String): Consequence[Short] =
    consequenceShortOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceShort(key: String, default: Short): Consequence[Short] =
    consequenceShortOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceIntOption(key: String): Consequence[Option[Int]] =
    get_Value(key).traverse(AnyUtils.consequenceInt)

  def consequenceInt(key: String): Consequence[Int] =
    consequenceIntOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceInt(key: String, default: Int): Consequence[Int] =
    consequenceIntOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceLongOption(key: String): Consequence[Option[Long]] =
    get_Value(key).traverse(AnyUtils.consequenceLong)

  def consequenceLong(key: String): Consequence[Long] =
    consequenceLongOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceLong(key: String, default: Long): Consequence[Long] =
    consequenceLongOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceFloatOption(key: String): Consequence[Option[Float]] =
    get_Value(key).traverse(AnyUtils.consequenceFloat)

  def consequenceFloat(key: String): Consequence[Float] =
    consequenceFloatOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceFloat(key: String, default: Float): Consequence[Float] =
    consequenceFloatOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceDoubleOption(key: String): Consequence[Option[Double]] =
    get_Value(key).traverse(AnyUtils.consequenceDouble)

  def consequenceDouble(key: String): Consequence[Double] =
    consequenceDoubleOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceDouble(key: String, default: Double): Consequence[Double] =
    consequenceDoubleOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceBigDecimalOption(key: String): Consequence[Option[BigDecimal]] =
    get_Value(key).traverse(AnyUtils.consequenceBigDecimal)

  def consequenceBigDecimal(key: String): Consequence[BigDecimal] =
    consequenceBigDecimalOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceBigDecimal(key: String, default: BigDecimal): Consequence[BigDecimal] =
    consequenceBigDecimalOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceStringOption(key: String): Consequence[Option[String]] =
    get_Value(key).traverse(AnyUtils.consequenceString)

  def consequenceString(key: String): Consequence[String] =
    consequenceStringOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceString(key: String, default: String): Consequence[String] =
    consequenceStringOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceUrlOption(key: String): Consequence[Option[URL]] =
    get_Value(key).traverse(AnyUtils.consequenceUrl)

  def consequenceUrl(key: String): Consequence[URL] =
    consequenceUrlOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }

  def consequenceUrl(key: String, default: URL): Consequence[URL] =
    consequenceUrlOption(key) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.success(default)
    }

  def consequenceTokenOption[T <: ValueInstance](key: String, f: ValueClass[T]): Consequence[Option[T]] =
    consequenceStringOption(key) flatMap {
      case Some(s) => f.get(s) match {
        case Some(v) => Consequence.success(Some(v))
        case None => Consequence.invalidTokenFault(key, s)
      }
      case None => Consequence.success(None)
    }

  def consequenceToken[T <: ValueInstance](key: String, f: ValueClass[T]): Consequence[T] =
    consequenceTokenOption(key,f) flatMap {
      case Some(s) => Consequence.success(s)
      case None => Consequence.missingPropertyFault(key)
    }
}
