package org.goldenport.values

import org.joda.time._

/*
 * @since   Aug. 26, 2015
 * @version Jan.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class RichDateTime(underlying: DateTime) extends AnyVal {
  def toRichDateTimeZone = RichDateTimeZone(underlying.getZone)
}
