package org.goldenport.util

import scala.util.control.NonFatal
import com.github.nscala_time.time.Imports._

/*
 * @since   Jun. 25, 2015
 *  version Sep. 25, 2015
 *  version Jan. 27, 2016
 *  version Mar. 19, 2016
 *  version Aug. 29, 2017
 *  version May. 23, 2018
 *  version Jun. 14, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
object LocalDateUtils {
  def parse(s: String): LocalDate = LocalDate.parse(s.trim)

  def parseYYYYMMDD(s: String): LocalDate = {
    // println(s"parseYYYYMMDD: $s")
    new LocalDate(DateUtils.parseYYYYMMDD(s), DateTimeUtils.jodagmt)
  }

  def parseYYYYMMDD(s: Short): LocalDate = parseYYYYMMDD(s.toString)
  def parseYYYYMMDD(s: Int): LocalDate = parseYYYYMMDD(s.toString)
  def parseYYYYMMDD(s: Long): LocalDate = parseYYYYMMDD(s.toString)

  def parse2(s: String): LocalDate =
    if (s.contains('/') || s.contains('-'))
      try {
        val x = s.replace('/', '-') // for excel YYYY/MM/DD
        parse(x)
      } catch {
        case NonFatal(e) => parseYYYYMMDD(s)
      }
    else
      parseYYYYMMDD(s)

  def dayCountInclusive(s: DateTime, e: DateTime): Int = {
    dayCountInclusive(s.toLocalDate, e.toLocalDate)
  }

  def dayCountInclusive(s: LocalDate, e: LocalDate): Int = {
    if (s < e)
      _day_count_inclusive(s, e)
    else
      _day_count_inclusive(e, s)
  }

  private def _day_count_inclusive(s: LocalDate, e: LocalDate): Int = {
    _day_count_exclusive(s, e, 0) + 1
  }

  def dayCountExclusive(s: DateTime, e: DateTime): Int = {
    dayCountExclusive(s.toLocalDate, e.toLocalDate)
  }

  def dayCountExclusive(s: LocalDate, e: LocalDate): Int = {
    if (s < e)
      _day_count_exclusive(s, e, 0)
    else
      _day_count_exclusive(e, s, 0)
  }

  @annotation.tailrec
  private def _day_count_exclusive(s: LocalDate, e: LocalDate, sum: Int): Int = {
    if (s.getYear == e.getYear) {
      e.getDayOfYear - s.getDayOfYear + sum
    } else {
      val s1 = new LocalDate(s.getYear, 12, 31)
      val a = s1.getDayOfYear - s.getDayOfYear + 1
      val s2 = new LocalDate(s.getYear + 1, 1, 1)
      _day_count_exclusive(s2, e, a + sum)
    }
  }

  def calcAge(base: LocalDate, date: LocalDate): Int = {
    require (date.compareTo(base) <= 0, "$base is less than $date")
    val dy = date.getYear
    val dm = date.getMonthOfYear
    val dd = date.getDayOfMonth
    val by = base.getYear
    val bm = base.getMonthOfYear
    val bd = base.getDayOfMonth
    val thisyear = if (bm == dm) dd <= bd else dm <= bm
    if (thisyear)
      by - dy
    else
      by - dy - 1
  }

  def ageToBirthdays(base: LocalDate, age: Int): (LocalDate, LocalDate) = {
    ageToBirthdays(base, age, age)
  }

  def ageToBirthdays(base: LocalDate, startage: Int, endage: Int): (LocalDate, LocalDate) = {
    require (startage <= endage, "start should be less equal than end")
    (base.minusYears(endage + 1).plusDays(1), base.minusYears(startage))
  }
}
