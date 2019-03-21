package org.goldenport.json

import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.goldenport.util.AnyUtils

/*
 * @since   Sep.  4, 2018
    case _: JsUndefined => None
 * @version Oct. 15, 2018
    case _: JsUndefined => None
 * @version Oct. 15, 2018
 * @author  ASAMI, Tomoharu
 */
case class JsonValue(o: JsValue) extends AnyVal {
  def asString(key: String): String = getString(key) getOrElse JsonUtils.raiseMissing(key)
  def getString(key: String): Option[String] = o match {
    case JsNull => None
    case JsBoolean(x) => Some(AnyUtils.toString(x))
    case JsNumber(x) => Some(AnyUtils.toString(x))
    case JsString(x) => Some(x)
    case x: JsObject => JsonUtils.raiseMismatch(key, o)
    case JsArray(xs) => JsonUtils.raiseMismatch(key, o)
  }
}

object JsonValue {
  object Implicits {
    implicit class JsonValueWrapper(val o: JsValue) extends AnyVal {
      def asString(key: String): String = o.asString(key)
      def getString(key: String): Option[String] = o.getString(key)
    }
  }
}
