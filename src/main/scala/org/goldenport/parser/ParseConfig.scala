package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 *  version Sep. 22, 2019
 * @version Jan. 23, 2021
 * @author  ASAMI, Tomoharu
 */
trait ParseConfig {
  def isLocation: Boolean
}

object ParseConfig {
  val empty = new ParseConfig() {
    def isLocation = false
  }
}

trait ForwardParseConfig extends ParseConfig {
  def config: ParseConfig

  def isLocation = config.isLocation
}
