package org.goldenport.util

/*
 * @since   Nov. 15, 2021
 * @version Nov. 15, 2021
 * @author  ASAMI, Tomoharu
 */
object IterableUtils {
  def enumDescription(ps: Iterable[Any]): String = enumDescriptionAnd(ps)

  def enumDescriptionAnd(ps: Iterable[Any]): String =
    EnumDescriptionMaker()(ps)

  def enumDescriptionOr(ps: Iterable[Any]): String =
    EnumDescriptionMaker(conjunction = "or")(ps)

  def enumDescriptionWithQuote(ps: Iterable[Any]): String =
    enumDescriptionWithQuoteAnd(ps)

  def enumDescriptionWithQuoteAnd(ps: Iterable[Any]): String =
    EnumDescriptionMaker(quote = Some('\''))(ps)

  def enumDescriptionWithQuoteOr(ps: Iterable[Any]): String =
    EnumDescriptionMaker(quote = Some('\''), conjunction = "or")(ps)

  case class EnumDescriptionMaker(
    quote: Option[Char] = None,
    conjunction: String = "and",
    format: Any => String = AnyUtils.toString(_)
  ) {
    def apply(ps: Iterable[Any]): String = ps.toList match {
      case Nil => "empty"
      case x :: Nil => _string(x)
      case xs => s"""${xs.init.map(_string).mkString(", ")} or ${_string(xs.last)}"""
    }

    private def _string(p: Any): String = quote.
      map(q => s"$q${format(p)}$q").
      getOrElse(format(p))
  }
}
