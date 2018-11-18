package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 * @version Nov. 18, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait ParseResult[AST] {
  def map[T](p: AST => T): ParseResult[T] = this.asInstanceOf[ParseResult[T]]
}

case class EmptyParseResult[AST]() extends ParseResult[AST]

case class ParseSuccess[AST](
  ast: AST,
  warnings: Vector[WarningMessage] = Vector.empty
) extends ParseResult[AST] {
  override def map[T](p: AST => T): ParseResult[T] = ParseSuccess[T](p(ast), warnings)
}

case class ParseFailure[AST](
  errors: Vector[ErrorMessage],
  warnings: Vector[WarningMessage]
) extends ParseResult[AST] {
}

object ParseResult {
  def empty[AST] = EmptyParseResult[AST]()
}
