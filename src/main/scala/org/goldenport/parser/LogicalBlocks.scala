package org.goldenport.parser

import org.goldenport.exception.RAISE

/*
 * @since   Aug. 20, 2018
 * @version Aug. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class LogicalBlocks(
  lines: Vector[LogicalLines]
)

object LogicalBlocks {
  def parse(in: Iterator[Char]) = RAISE.notImplementedYetDefect
}
