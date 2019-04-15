package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 30, 2018
 *  version Sep.  9, 2018
 *  version Oct. 26, 2018
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
trait LogicalTokenReaderWriterState[C <: ParseConfig, AST] {
  def apply(config: C, event: LogicalToken): (ParseMessageSequence, ParseResult[AST], LogicalTokenReaderWriterState[C, AST])
}

case class LogicalTokenReaderWriterStateClass[C <: ParseConfig, AST](
  config: C,
  init: LogicalTokenReaderWriterState[C, AST]
) {
  type S = LogicalTokenReaderWriterState[C, AST]
  type B = ParseResult[AST]
  type RWS = ReaderWriterState[C, ParseMessageSequence, S, B]
  type OUT = (ParseMessageSequence, B, S)

  def initrws: RWS = ReaderWriterState((c, s) => (ParseMessageSequence.empty, ParseResult.empty, init))

  def action(event: LogicalToken): RWS = ReaderWriterState((c, s) => s.apply(c, event))

  def apply(s: String): OUT = {
    val tokens = LogicalTokens.parse(s)
    val r = _parse_tokens(tokens.tokensWithEnd)
    // println(s"LogicalTokenReaderWriterState: $s => $tokens => $r")
    r
  }

  def applySExpr(s: String): OUT = {
    val c = LogicalTokens.Config.sexpr
    val tokens = LogicalTokens.parse(c, s)
    val r = _parse_tokens(tokens.tokensWithEnd)
    // println(s"LogicalTokenReaderWriterState#applySExpr: $s => $tokens => $r")
    r
  }

  private def _parse_tokens(tokens: Seq[LogicalToken]): OUT = {
    val a = tokens./:(initrws)((z, x) =>
      for {
        _ <- z
        r <- action(x)
      } yield r
    )
    a.run(config, init)
  }
}
