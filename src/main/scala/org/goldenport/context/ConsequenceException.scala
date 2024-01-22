package org.goldenport.context

import org.goldenport.exception.GoldenportException

/*
 * @since   May. 30, 2021
 * @version Nov. 15, 2021
 * @author  ASAMI, Tomoharu
 */
class ConsequenceException(val consequence: Consequence[_])
    extends GoldenportException(consequence.conclusion.message) {
  def message = consequence.conclusion.message
}
