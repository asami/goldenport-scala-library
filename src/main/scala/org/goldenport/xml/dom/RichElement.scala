package org.goldenport.xml.dom

import scala.language.implicitConversions
import org.w3c.dom._

/*
 * @since   Apr. 11, 2016
 *  version Oct. 12, 2017
 *  version May. 27, 2019
 * @version May.  6, 2022
 * @author  ASAMI, Tomoharu
 */
case class RichElement(element: Element) extends AnyVal {
  def localName: String = DomUtils.localName(element)

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

  def getElementByLocalNameIC(localname: String): Option[Element] = DomUtils.getElementByLocalNameIC(element, localname)

  def getElementByLocalNameIC(localname: String, localname2: String, localnames: String*): Option[Element] = DomUtils.getElementByLocalNameIC(element, localname, localname2, localnames:_*)

  def elements: List[Element] = DomUtils.elements(element)

  def elementsByLocalNameIC(localname: String): List[Element] = DomUtils.elementsByLocalNameIC(element, localname)

  def elementsByLocalNameIC(localname: String, localname2: String, localnames: String*): List[Element] = DomUtils.elementsByLocalNameIC(element, localname, localname2, localnames:_*)

  def elementsVectorByLocalNameIC(localname: String): Vector[Element] = DomUtils.elementsVectorByLocalNameIC(element, localname)

  def elementsVectorByLocalNameIC(localname: String, localname2: String, localnames: String*): Vector[Element] = DomUtils.elementsVectorByLocalNameIC(element, localname, localname2, localnames:_*)

  def text: String = DomUtils.distillText(element)
}

object RichElement {
  object Implicits {
    implicit def enrich(element: Element) = new RichElement(element)
  }
}
