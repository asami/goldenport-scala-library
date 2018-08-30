package org.goldenport.parser

import org.w3c.dom._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 28, 2018
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class DomParser() extends Parser {
}

object DomParser {
  implicit class DomParser(val sc: StringContext) extends AnyVal {
    def dom(args: Any*): Node = RAISE.notImplementedYetDefect
  }
}
