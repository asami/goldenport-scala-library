package org.goldenport.extension

import org.apache.commons.jxpath.DynamicPropertyHandler
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.util.AnyRefUtils

/*
 * @since   Oct. 15, 2018
 * @version Oct. 15, 2018
 * @author  ASAMI, Tomoharu
 */
case class RecordDynamicPropertyHandler() extends DynamicPropertyHandler {
  println(s"RecordDynamicPropertyHandler")

  def getPropertyNames(o: AnyRef): Array[String] = {
    val r: Array[String] = o match {
      case m: IRecord => m.keyNames.toArray
      case m => Array()
    }
    println(s"RecordDynamicPropertyHandler: $o => $r")
    r
  }

  def getProperty(o: AnyRef, name: String): AnyRef = {
    val r = o match {
      case m: IRecord => m.get(name) match {
        case Some(s) => s
        case None => null
      }
      case m => null
    }
    println(s"RecordDynamicPropertyHandler: $o/$name => $r")
    AnyRefUtils.toAnyRef(r)
  }

  def setProperty(o: AnyRef, name: String, value: AnyRef): Unit = {
    RAISE.unsupportedOperationFault
  }
}
object RecordDynamicPropertyHandler {
}
