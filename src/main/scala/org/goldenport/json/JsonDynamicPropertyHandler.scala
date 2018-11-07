package org.goldenport.json

import org.apache.commons.jxpath.DynamicPropertyHandler
import play.api.libs.json._
import org.goldenport.RAISE

/*
 * @since   Oct. 14, 2018
 * @version Oct. 14, 2018
 * @author  ASAMI, Tomoharu
 */
case class JsonDynamicPropertyHandler() extends DynamicPropertyHandler {
  println(s"JsonDynamicPropertyHandler")

  def getPropertyNames(o: AnyRef): Array[String] = {
    val r: Array[String] = o match {
      case m: JsObject => m.fields.map(_._1).toArray
      case m => Array()
    }
    println(s"JsonDynamicPropertyHandler: $o => $r")
    r
  }

  def getProperty(o: AnyRef, name: String): AnyRef = {
    val r = o match {
      case m: JsObject => m.fields.find(_._1 == name) match {
        case Some(s) => s._2 match {
          case m: JsString => m.value
          case m: JsBoolean => m.value: java.lang.Boolean
          case m: JsNumber => m.value
          case m: JsObject => m
          case m: JsArray => m.value.toArray
          case m: JsUndefined => null
          case JsNull => null
        }
        case None => null
      }
      case m => null
    }
    println(s"JsonDynamicPropertyHandler: $o/$name => $r")
    r
  }

  def setProperty(o: AnyRef, name: String, value: AnyRef): Unit = {
    RAISE.unsupportedOperationFault
  }
}
object JsonDynamicPropertyHandler {
}
