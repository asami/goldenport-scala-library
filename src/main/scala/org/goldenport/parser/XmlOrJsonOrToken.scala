package org.goldenport.parser

/*
 * @since   Jul.  7, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait XmlOrJsonOrToken {
  def string: String
}

object XmlOrJsonOrToken {
  def create(p: String): XmlOrJsonOrToken = p.headOption.map {
    case '<' => XmlXmlOrJsonOrToken(p)
    case '{' => JsonXmlOrJsonOrToken(p)
    case '"' => StringXmlOrJsonOrToken(p)
    case _ => TokenXmlOrJsonOrToken(p)
  }.getOrElse(EmptyXmlOrJsonOrToken)
}

case object EmptyXmlOrJsonOrToken extends XmlOrJsonOrToken {
  def string: String = ""
}

case class XmlXmlOrJsonOrToken(text: String) extends XmlOrJsonOrToken {
  def string: String = text
}

case class JsonXmlOrJsonOrToken(text: String) extends XmlOrJsonOrToken {
  def string: String = text
}

case class StringXmlOrJsonOrToken(text: String) extends XmlOrJsonOrToken {
  def string: String = text // TODO
}

case class TokenXmlOrJsonOrToken(text: String) extends XmlOrJsonOrToken {
  def string: String = text // xXX
}
