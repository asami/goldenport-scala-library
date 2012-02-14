package org.goldenport.util

import java.util._

/**
 * @since   Sep.  6. 2010
 * @version Feb. 14, 2012
 * @author  ASAMI, Tomoharu
 */
object VDate {
  def apply(): VDate = apply(new java.util.Date)

  def apply(d: java.util.Date): VDate = apply(d, "GMT")

  def apply(d: java.util.Date, tzs: String): VDate = {
    val df = javax.xml.datatype.DatatypeFactory.newInstance
    val tz = TimeZone.getTimeZone(tzs)
    val cal = df.newXMLGregorianCalendar(new java.util.GregorianCalendar(tz))
    new VDate(cal.getYear, cal.getMonth, cal.getDay)
  }
}

object VDateTime {
  def apply(): VDateTime = apply(new java.util.Date)

  def apply(d: java.util.Date): VDateTime = apply(d, "GMT")

  def apply(d: java.util.Date, tzs: String): VDateTime = {
    val df = javax.xml.datatype.DatatypeFactory.newInstance
    val tz = TimeZone.getTimeZone(tzs)
    val cal = df.newXMLGregorianCalendar(new java.util.GregorianCalendar(tz))
    new VDateTime(cal.getYear, cal.getMonth, cal.getDay,
                  cal.getHour, cal.getMinute, cal.getSecond,
                  Some(cal.getMillisecond()), Some(tz))
  }

  def apply(s: String): VDateTime = {
//println("VDateTime s = " + s)
    val regex = """(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)(.(\d\d\d))?(.*)?""".r
    val regex(y, m, d, hr, mi, sc, _, msc, tz) = s
    new VDateTime(y.toInt, m.toInt, d.toInt,
                  hr.toInt, mi.toInt, sc.toInt,
                  if (msc != null) Some(msc.toInt) else None,
                  if (tz == null || tz == "") None else Some(TimeZone.getTimeZone(tz)))
  }

  def apply(l: Long): VDateTime = apply(new java.util.Date(l))
}

object VTime {
  def apply(): VTime = apply(new java.util.Date)

  def apply(d: java.util.Date): VTime = apply(d, "GMT")

  def apply(d: java.util.Date, tzs: String): VTime = {
    val df = javax.xml.datatype.DatatypeFactory.newInstance
    val tz = TimeZone.getTimeZone(tzs)
    val cal = df.newXMLGregorianCalendar(new java.util.GregorianCalendar(tz))
    new VTime(cal.getHour, cal.getMinute, cal.getSecond,
              Some(cal.getMillisecond()), Some(tz))
  }
}

trait DataValue {
  def sqlLiteral: String
}

class VDate(val year: Int, val month: Int, val day: Int) extends DataValue {
  def sqlLiteral = "'%04d-%02d-%02d'".format(year, month, day)

  override def toString() = "%04d-%02d-%02d".format(year, month, day)
}

class VTime(val hour: Int, val minute: Int, val second: Int,
            val millsecond: Option[Int] = None,
            val timezone: Option[TimeZone] = None) extends DataValue with VTimeUtil {
  def sqlLiteral = "'%02d:%02d:%02d'".format(hour, minute, second)

  override def toString() = {
    millsecond match {
      case Some(ms) => "%02d:%02d:%02d.%03d%s".format(hour, minute, second, ms, tz_string)
      case None => "%02d:%02d:%02d%s".format(hour, minute, second, tz_string)
    }
  }
}

class VDateTime(val year: Int, val month: Int, val day: Int,
                val hour: Int, val minute: Int, val second: Int,
                val millsecond: Option[Int] = None,
                val timezone: Option[TimeZone] = None) extends DataValue with VTimeUtil {
  def sqlLiteral = "'%04d-%02d-%02d %02d:%02d:%02d'".format(year, month, day, hour, minute, second)

  def toDate: java.util.Date = {
    val cal = new GregorianCalendar()
    cal.set(year, month, day, hour, minute, second)
    for (m <- millsecond) {
      cal.set(Calendar.MILLISECOND, m)
    }
    for (tz <- timezone) {
      cal.setTimeZone(tz)
    }
    cal.getTime
  }

  override def toString() = {
    millsecond match {
      case Some(ms) => "%04d-%02d-%02dT%02d:%02d:%02d.%03d%s".format(year, month, day, hour, minute, second, ms, tz_string)
      case None => "%04d-%02d-%02dT%02d:%02d:%02d%s".format(year, month, day, hour, minute, second, second, tz_string)
    }
  }
}

trait VTimeUtil {
  val timezone: Option[TimeZone]

  protected final def tz_string = timezone match {
    case Some(tz) if tz.getID == "GMT" => "Z"
    case Some(tz) => tz.getID
    case None => "Z"
  }
}
