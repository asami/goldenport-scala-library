package org.goldenport.util

import scala.concurrent.duration._

/*
 * @since   Oct. 22, 2020
 * @version Oct. 22, 2020
 * @author  ASAMI, Tomoharu
 */
object DurationUtils {
  def isIn[T](base: Long, duration: Duration, f: T => Long)(p: T): Boolean =
    if (duration.isFinite)
      f(p) >= base - duration.toMillis
    else
      true
}
