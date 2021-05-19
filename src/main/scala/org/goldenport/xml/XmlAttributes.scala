package org.goldenport.xml

import scala.xml._
import org.xml.sax._
import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * @since   Feb. 22, 2016
 *  version Apr. 18, 2016
 *  version Oct. 12, 2017
 * @version Feb.  5, 2021
 * @author  ASAMI, Tomoharu
 */
case class XmlAttributes(attributes: Vector[XmlAttribute]) extends Attributes {
  def +(attr: XmlAttribute): XmlAttributes =
    copy(attributes = attributes :+ attr)

  def getIndex(qname: String): Int =
    attributes.indexWhere(_.qName == qname)
  def getIndex(uri: String, localname: String): Int =
    attributes.indexWhere(x => x.uri == Some(uri) && x.localName == localname)
  def getLength(): Int = attributes.length
  def getLocalName(index: Int): String = _get(index)(_.localName)
  def getQName(index: Int): String = _get(index)(_.qName)
  def getType(qname: String): String = _get(qname)(_.xmlType)
  def getType(uri: String, localname: String): String = _get(uri, localname)(_.xmlType)
  def getType(index: Int): String = _get(index)(_.xmlType)
  def getURI(index: Int): String = _get(index)(_.uri getOrElse "")
  def getValue(qname: String): String = _get(qname)(_.value)
  def getValue(uri: String, localname: String): String = _get(uri, localname)(_.value)
  def getValue(index: Int): String = _get(index)(_.value)

  private def _get(index: Int)(f: XmlAttribute => String): String =
    attributes.lift(index) match {
      case Some(s) => f(s) // if (s.uri.isEmpty) "" else f(s)
      case None => null
    }

  private def _get(qname: String)(f: XmlAttribute => String): String =
    attributes.find(_.qName == qname) match {
      case Some(s) => f(s) // if (s.uri.isEmpty) "" else f(s)
      case None => null
    }

  private def _get(uri: String, localname: String)(f: XmlAttribute => String): String =
    attributes.find(x => x.uri == uri && x.localName == localname) match {
      case Some(s) => f(s) // if (s.uri.isEmpty) "" else f(s)
      case None => null
    }
}

object XmlAttributes {
  val empty = XmlAttributes(Vector.empty)

  def create(ps: Seq[Tuple2[String, String]]): XmlAttributes = XmlAttributes(
    ps.toVector.map(XmlAttribute.create)
  )
}

case class XmlAttribute(
  localName: String,
  qName: String,
  prefix: Option[String],
  uri: Option[String],
  value: String
) {
  def xmlType = "CDATA"
}

object XmlAttribute {
  def create(ns: NamespaceBinding, attr: MetaData): Either[NamespaceBinding, XmlAttribute] = {
    val v = XmlUtils.toString(attr.value)
    Strings.totokens(attr.key, ":") match {
      case Nil => throw new IllegalArgumentException("Invalid attribute")
      case name :: Nil => Right(XmlAttribute(name, name, None, None, v))
      case "xmlns" :: prefix :: _ => Left(new NamespaceBinding(prefix, v, ns))
      case prefix :: name :: _ => Option(ns.getURI(prefix)).fold(
        Right(XmlAttribute(name, attr.key, Some(prefix), None, v))
      ) { uri =>
        Right(XmlAttribute(name, attr.key, Some(prefix), Some(uri), v))
      }
    }
  }

  def create(p: Tuple2[String, String]): XmlAttribute = {
    val (qname, value) = p
    Strings.totokens(qname, ":") match {
      case Nil => RAISE.invalidArgumentFault("Empty attribute name")
      case name :: Nil => XmlAttribute(name, name, None, None, value)
      case prefix :: name :: _ => XmlAttribute(name, qname, Some(prefix), None, value)
    }
  }
}
