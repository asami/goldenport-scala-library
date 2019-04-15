package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Oct. 14, 2018
 *  version Nov. 11, 2018
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
trait LogicalBlockReaderWriterState[C <: ParseConfig, AST] {
  def result: AST
  def apply(config: C, event: LogicalBlock): (ParseMessageSequence, ParseResult[AST], LogicalBlockReaderWriterState[C, AST])
}

case class LogicalBlockReaderWriterStateClass[C <: ParseConfig, AST](
  config: C,
  init: LogicalBlockReaderWriterState[C, AST]
) {
  type S = LogicalBlockReaderWriterState[C, AST]
  type B = ParseResult[AST]
  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: LogicalBlock): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  def apply(s: String): OUT = {
    val blocks = LogicalBlocks.parse(s)
    val r = _parse_blocks(blocks.events)
    // println(s"LogicalBlockReaderWriterStateClass: ${blocks} => $r")
    r
  }

  private def _parse_blocks(blocks: Seq[LogicalBlock]): OUT = {
    val a = blocks./:(initrws)((z, x) =>
      for {
        _ <- z
        r <- action(x)
      } yield {
        // println(s"LogicalBlockReaderWriterStateClass#_parse_blocks: $r")
        r
      }
    )
    a.run(config, init)
  }
}
