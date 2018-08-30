package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 28, 2018
 * @version Aug. 28, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait Tokernizer {
}

trait SimpleTokenizer extends Tokernizer {
}

trait ComplexTokenizer extends Tokernizer {
}
