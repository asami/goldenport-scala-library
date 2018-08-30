package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseConfig {
}

object ParseConfig {
  val empty = new ParseConfig() {}
}
