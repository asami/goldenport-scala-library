package org.goldenport.xml.dom

import scala.language.implicitConversions
import org.w3c.dom._

/*
 * @since   Apr. 11, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
case class RichElement(element: Element) extends AnyVal {
  def isAttributeIC(name: String): Boolean = getAttributeOIC(name).isDefined

  def getAttributeOIC(name: String): Option[String] =
    DomUtils.getAttributeIgnoreCase(element, name)

  def complementAttributeOIC(name: String, f: => String): Element = {
    if (isAttributeIC(name)) {
      element.setAttribute(name, f)
      element
    } else
      element
  }
}

object RichElement {
  object Implicits {
    implicit def enrich(element: Element) = new RichElement(element)
  }
}
