package org.goldenport.xml.dom

import org.w3c.dom._
import org.goldenport.exception.GoldenportException

/*
 * @since   Nov. 28, 2020
 * @version Nov. 28, 2020
 * @author  ASAMI, Tomoharu
 */
abstract class DomException(msg: String, cause: Throwable = null) extends GoldenportException(msg, cause) {
}

class DomElementException(name: String, cause: Throwable = null) extends DomException(s"Element name: $name", cause)
