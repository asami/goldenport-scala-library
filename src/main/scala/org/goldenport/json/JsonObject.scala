package org.goldenport.json

import play.api.libs.json._
import play.api.libs.functional.syntax._

/*
 * @since   Sep.  4, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
case class JsonObject(o: JsObject) extends AnyVal {
  def asString(key: String): String = JsonUtils.toString(key, o)
  def getString(key: String): Option[String] = JsonUtils.getString(o, key)
}

object JsonObject {
  object Implicits {
    implicit class JsonObjectWrapper(val o: JsObject) extends AnyVal {
      def asString(key: String): String = JsonObject(o).asString(key)
      def getString(key: String): Option[String] = JsonObject(o).getString(key)
    }
  }
}
