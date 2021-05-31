package org.goldenport.context

import org.goldenport.exception.GoldenportException

/*
 * @since   May. 30, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
class ConsequenceException[T](consequence: Consequence[T]) extends GoldenportException(consequence.conclusion.message.en) {
}
