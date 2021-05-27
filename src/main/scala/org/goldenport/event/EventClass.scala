package org.goldenport.event

import org.goldenport.RAISE
import org.goldenport.parser.ParseResult

/*
 * @since   May.  3, 2021
 * @version May. 23, 2021
 * @author  ASAMI, Tomoharu
 */
case class EventClazz(
  name: String
) {
  def create(): Event = SignalEvent(this, Signal.none)
}

object EventClazz {
  def parse(p: String): ParseResult[EventClazz] = RAISE.notImplementedYetDefect
}
