package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 21, 2018
 *  version Nov. 18, 2018
 *  version Dec.  2, 2018
 * @version Feb.  2, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait ParseResult[AST] {
  def map[T](p: AST => T): ParseResult[T] = this.asInstanceOf[ParseResult[T]]
  def get: Option[AST]
  def errors: Vector[ErrorMessage]
  def warnings: Vector[WarningMessage]
  def messages: Vector[ParseMessage] = errors ++ warnings
}

case class EmptyParseResult[AST]() extends ParseResult[AST] {
  def get = None
  def errors = Vector.empty
  def warnings = Vector.empty
}

case class ParseSuccess[AST](
  ast: AST,
  warnings: Vector[WarningMessage] = Vector.empty
) extends ParseResult[AST] {
  override def map[T](p: AST => T): ParseResult[T] = ParseSuccess[T](p(ast), warnings)
  def errors = Vector.empty
  def get = Some(ast)
}

case class ParseFailure[AST](
  errors: Vector[ErrorMessage],
  warnings: Vector[WarningMessage]
) extends ParseResult[AST] {
  def get = None
}

object ParseResult {
  def empty[AST] = EmptyParseResult[AST]()

  def error[AST](en: String, ja: String, location: Option[ParseLocation]): ParseFailure[AST] =
    ParseFailure(Vector(ErrorMessage(en, ja, location)), Vector.empty)
}
