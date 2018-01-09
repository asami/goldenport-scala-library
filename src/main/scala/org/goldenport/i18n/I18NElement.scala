package org.goldenport.i18n

import scalaz.{Node =>_, _} , Scalaz._
import java.util.{Locale, ResourceBundle}
import scala.xml._
import org.goldenport.xml.XmlPrinter
import org.goldenport.xml.XmlUtils.{makeString, parseNodeSeq}

/*
 * @since   Aug. 13, 2017
 *  version Sep. 11, 2017
 *  version Oct. 30, 2017
 *  version Nov.  1, 2017
 * @version Jan.  5, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait I18NElement {
  def apply(locale: Locale): NodeSeq
  def get(locale: Locale): Option[NodeSeq]
  def toI18NString: I18NString
  def toJsonString: String
}

object I18NElement {
  val blank = apply("")
  def parse(s: String): I18NElement = I18NStringI18NElement(I18NString.parse(s))

  def apply(en: String): I18NElement = I18NStringI18NElement(I18NString(en))

  def apply(en: String, ja: String): I18NElement = I18NStringI18NElement(I18NString(en, ja))

  def apply(p: NodeSeq): I18NElement = p match {
    case m: NodeSeq => NodeSeqI18NElement(m)
    case _ => I18NElement(p.toString)
  }
}

case class NodeSeqI18NElement(v: Map[Locale, NodeSeq]) extends I18NElement {
  import I18NElement._

  lazy val en: NodeSeq = v.get(Locale.ENGLISH) getOrElse Text("")
  lazy val ja: NodeSeq = v.get(Locale.JAPANESE) getOrElse en
  def apply(locale: Locale): NodeSeq = v.get(locale) getOrElse en
  def get(locale: Locale): Option[NodeSeq] = v.get(locale) // TODO country/variation. See I18NString.
  def toI18NString: I18NString = I18NString(v.mapValues(x => XmlPrinter.html(x)).toVector)
  def toJsonString = toI18NString.toJsonString
}
object NodeSeqI18NElement {
  def apply(p: NodeSeq): NodeSeqI18NElement = NodeSeqI18NElement(
    Map(Locale.ENGLISH -> p)
  )
}

case class I18NStringI18NElement(v: I18NString) extends I18NElement {
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
