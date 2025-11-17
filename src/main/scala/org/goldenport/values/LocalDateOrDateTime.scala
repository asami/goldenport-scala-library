package org.goldenport.values

import org.joda.time.{LocalDate, DateTime}
import org.goldenport.context.Consequence
import org.goldenport.context.DateTimeContext
import org.goldenport.datatype.Datatype
import org.goldenport.util.LocalDateUtils
import org.goldenport.util.DateTimeUtils
import org.goldenport.util.AnyUtils

/*
 * @since   Jun. 16, 2025
 * @version Nov. 17, 2025
 * @author  ASAMI, Tomoharu
 */
case class LocalDateOrDateTime(either: Either[LocalDate, DateTime]) extends Datatype {
  protected def print_String = either.fold(AnyUtils.toString, AnyUtils.toString)

  def toLocalDate: LocalDate = either.fold(identity, _.toLocalDate)
}

object LocalDateOrDateTime {
  import io.circe._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._

  def apply(p: LocalDate): LocalDateOrDateTime = LocalDateOrDateTime(Left(p))

  def apply(p: DateTime): LocalDateOrDateTime = LocalDateOrDateTime(Right(p))

  def parse(s: String)(implicit dctx: DateTimeContext): Consequence[LocalDateOrDateTime] = {
    LocalDateUtils.consequenceLocalDate(s).fold(
      _ => DateTimeUtils.consequenceDateTimeWithContext(s).map(apply),
      x => Consequence(apply(x))
    ).onError(_ => Consequence.formatErrorFault(s"Invalid LocalDate or DateTime: $s"))
  }

  def parseStatic(s: String): Consequence[LocalDateOrDateTime] =
    parse(s)(DateTimeContext.now())

  implicit val ordering: Ordering[LocalDateOrDateTime] = new Ordering[LocalDateOrDateTime] {
    override def compare(x: LocalDateOrDateTime, y: LocalDateOrDateTime): Int = {
      val xt = x.either.fold(_.toDateTimeAtStartOfDay(), identity)
      val yt = y.either.fold(_.toDateTimeAtStartOfDay(), identity)
      xt.compareTo(yt)
    }
  }

  implicit val localDateOrDateTimeEncoder: Encoder[LocalDateOrDateTime] =
    Encoder.encodeString.contramap(_.toString)
}
