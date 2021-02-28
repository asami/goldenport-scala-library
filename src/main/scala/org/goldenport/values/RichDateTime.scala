package org.goldenport.values

import org.joda.time._

/*
 * @since   Aug. 26, 2015
 *  version Jan.  9, 2019
 * @version Jan. 24, 2021
 * @author  ASAMI, Tomoharu
 */
case class RichDateTime(underlying: DateTime) extends AnyVal {
  def toRichDateTimeZone = RichDateTimeZone(underlying.getZone)

  def currentYear: Int = underlying.year.get
  def currentMonth: Int = underlying.monthOfYear.get
  def currentWeek: Int = underlying.weekOfWeekyear.get
  def currentDay: Int = underlying.dayOfMonth.get
  def currentHour: Int = underlying.hourOfDay.get
}

object RichDateTime {
  import scala.language.implicitConversions

  object Implicits {
    implicit def enrichDateTime(p: DateTime) = new RichDateTime(p)
  }
}
