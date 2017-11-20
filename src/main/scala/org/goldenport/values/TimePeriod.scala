package org.goldenport.values

import org.joda.time.LocalTime

/*
 * @since   Nov. 18, 2017
 * @version Nov. 18, 2017
 * @author  ASAMI, Tomoharu
 */
case class TimePeriod(
  start: LocalTime,
  end: LocalTime,
  endInclusion: Boolean
)
