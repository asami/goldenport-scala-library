package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 * @version Aug. 26, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait ParseResult[AST]

case class EmptyParseResult[AST]() extends ParseResult[AST]

case class ParseSuccess[AST](
  ast: AST,
  warnings: Vector[WarningMessage] = Vector.empty
) extends ParseResult[AST] {
}

case class ParseFailure[AST](
  errors: Vector[ErrorMessage],
  warnings: Vector[WarningMessage]
) extends ParseResult[AST] {
}

object ParseResult {
  def empty[AST] = EmptyParseResult[AST]()
}
