package org.goldenport.parser

import org.goldenport.exception.RAISE

/*
 * @since   Sep.  1, 2018
 * @version Sep.  1, 2018
 * @author  ASAMI, Tomoharu
 */
case class XPathParser() extends Parser {
}

object XPathParser {
  implicit class XPathParser(val sc: StringContext) extends AnyVal {
    def dom(args: Any*): Any = RAISE.notImplementedYetDefect
  }
}
