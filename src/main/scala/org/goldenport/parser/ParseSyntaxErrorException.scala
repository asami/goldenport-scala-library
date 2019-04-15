package org.goldenport.parser

import org.goldenport.exception.GoldenportException

/*
 * @since   Feb.  2, 2019
 * @version Feb.  2, 2019
 * @author  ASAMI, Tomoharu
 */
class ParseSyntaxErrorException(
  messags: String,
  errors: Vector[ErrorMessage],
  wranings: Vector[WarningMessage]
) extends GoldenportException(messags) {
}

object ParseSyntaxErrorException {
  def apply(errors: Vector[ErrorMessage], warnings: Vector[WarningMessage]): ParseSyntaxErrorException = {
    val msg = if (errors.nonEmpty)
      errors.map(_.en).mkString("\n")
    else if (warnings.nonEmpty)
      warnings.map(_.en).mkString("\n")
    else
      ""
    new ParseSyntaxErrorException(msg, errors, warnings)
  }
}
