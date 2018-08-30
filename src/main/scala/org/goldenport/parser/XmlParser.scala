package org.goldenport.parser

import scala.xml._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 19, 2018
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class XmlParser() extends Parser {
}

object XmlParser {
  implicit class XmlParser(val sc: StringContext) extends AnyVal {
    def xml(args: Any*): Node = RAISE.notImplementedYetDefect
  }
}
