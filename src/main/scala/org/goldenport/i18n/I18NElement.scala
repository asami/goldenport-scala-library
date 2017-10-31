package org.goldenport.i18n

import scalaz.{Node =>_, _} , Scalaz._
import java.util.{Locale, ResourceBundle}
import scala.xml._
import org.goldenport.xml.XmlUtils

/*
 * @since   Aug. 13, 2017
 *  version Sep. 11, 2017
 * @version Oct. 30, 2017
 * @author  ASAMI, Tomoharu
 */
case class I18NElement(v: I18NString) {
  import I18NElement._
  lazy val en: NodeSeq = parseNodeSeq(v.en)
  lazy val ja: NodeSeq = parseNodeSeq(v.ja)
  def apply(locale: Locale): NodeSeq = get(locale) getOrElse en
  def get(locale: Locale): Option[NodeSeq] = v.get(locale).map(parseNodeSeq)
  def toJson = v.toJson
  def toJsonString = v.toJsonString
  lazy val toI18NString = I18NString(
    makeString(v._en),
    makeString(v._ja),
    v.map.mapValues(makeString),
    v.parameters
  )

  override def toString() = toJsonString
}

object I18NElement {
  def parse(s: String): I18NElement = I18NElement(I18NString.parse(s))

  def apply(en: String): I18NElement = I18NElement(I18NString(en))

  def apply(en: String, ja: String): I18NElement = I18NElement(I18NString(en, ja))

  def apply(p: NodeSeq): I18NElement = I18NElement(p.toString)

  private def parseNodeSeq(p: String): NodeSeq = XmlUtils.parseNodeSeq(p)
  private def makeString(p: String): String = XmlUtils.makeString(p)
}
