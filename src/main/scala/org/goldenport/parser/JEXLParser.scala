package org.goldenport.parser

import org.goldenport.exception.RAISE

/*
 * @since   Sep.  1, 2018
 * @version Sep.  1, 2018
 * @author  ASAMI, Tomoharu
 */
case class JEXLParser() extends Parser {
}

object JEXLParser {
  implicit class JEXLParser(val sc: StringContext) extends AnyVal {
    def dom(args: Any*): Any = RAISE.notImplementedYetDefect
  }
}
