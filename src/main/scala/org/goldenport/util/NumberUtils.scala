package org.goldenport.util

import scala.util.control.NonFatal
import java.math.MathContext
import java.math.RoundingMode
import org.goldenport.Strings
import org.goldenport.context.Consequence
import org.goldenport.parser.ParseResult

/*
 * @since   Mar. 12, 2020
 *  version Apr. 21, 2020
 *  version Sep. 29, 2020
 *  version Oct. 20, 2020
 *  version Jan. 19, 2021
 *  version Feb. 14, 2021
 *  version Mar. 24, 2021
 *  version Nov.  5, 2021
 *  version Jan. 27, 2022
 *  version Feb. 16, 2022
 *  version Mar. 19, 2022
 *  version Jul. 22, 2023
 * @version Oct.  9, 2024
 * @author  ASAMI, Tomoharu
 */
object NumberUtils {
  val BIGDECIMAL_ZERO = BigDecimal(0)

  def getBoolean(p: Option[String]): Option[Boolean] = p.flatMap(getBoolean)

  def getBoolean(p: String): Option[Boolean] = p.trim.toLowerCase match {
    case "1" => Some(true)
    case "0" => Some(false)
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }

  def getByte(p: Option[String]): Option[Byte] = p.flatMap(getByte)

  def getByte(p: String): Option[Byte] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toByte)
  } catch {
    case e: NumberFormatException => None
  }

  def optionByte(p: Any): Option[Byte] = consequenceByte(p).toOption

  def getShort(p: Option[String]): Option[Short] = p.flatMap(getShort)

  def getShort(p: String): Option[Short] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toShort)
  } catch {
    case e: NumberFormatException => None
  }

  def optionShort(p: Any): Option[Short] = consequenceShort(p).toOption

  def getInt(p: Option[String]): Option[Int] = p.flatMap(getInt)

  def getInt(p: String): Option[Int] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toInt)
  } catch {
    case e: NumberFormatException => None
  }

  def optionInt(p: Any): Option[Int] = consequenceInt(p).toOption

  def getLong(p: Option[String]): Option[Long] = p.flatMap(getLong)

  def getLong(p: String): Option[Long] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toLong)
  } catch {
    case e: NumberFormatException => None
  }

  def optionLong(p: Any): Option[Long] = consequenceLong(p).toOption

  def getFloat(p: Option[String]): Option[Float] = p.flatMap(getFloat)

  def getFloat(p: String): Option[Float] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toFloat)
  } catch {
    case e: NumberFormatException => None
  }

  def optionFloat(p: Any): Option[Float] = consequenceFloat(p).toOption

  def getDouble(p: Option[String]): Option[Double] = p.flatMap(getDouble)

  def getDouble(p: String): Option[Double] = try {
    if (Strings.blankp(p))
      None
    else
      Some(p.trim.toDouble)
  } catch {
    case e: NumberFormatException => None
  }

  def optionDouble(p: Any): Option[Double] = consequenceDouble(p).toOption

  def optionBigInt(p: Any): Option[BigInt] = consequenceBigInt(p).toOption

  def optionBigDecimal(p: Any): Option[BigDecimal] = consequenceBigDecimal(p).toOption

  def toBigDecimal(p: Any): BigDecimal = consequenceBigDecimal(p).take

  def charToInt(p: Char): Int = {
    val v = p - '0'
    if (v > 9)
      throw new NumberFormatException(p.toString)
    else
      v
  }

  def parse(p: String): ParseResult[Number] = try {
    ParseResult.success(AnyUtils.toNumber(p))
  } catch {
    case NonFatal(e) => ParseResult.error(s"Invalid number: $p")
  }

  def parse(labelf: String => Option[Number], p: String): ParseResult[Number] = {
    def msg = s"Invalid label or number: $p"
    for {
      a <- ParseResult.executeDebug[Option[Number]](msg)(labelf(p))
      b <- a.map(ParseResult.success(_)) getOrElse {
        try {
          ParseResult.success(AnyUtils.toNumber(p))
        } catch {
          case NonFatal(e) => ParseResult.error(msg)
        }
      }
    } yield b
  }

  def parseInt(p: Option[String]): ParseResult[Int] =
    p.map(parseInt).getOrElse(ParseResult.empty)

  def parseInt(p: Option[String], default: Int): ParseResult[Int] =
    p.map(parseInt).getOrElse(ParseResult.success(default))

  def parseInt(p: String): ParseResult[Int] = try {
    if (Strings.blankp(p))
      ParseResult.empty
    else
      ParseResult.success(p.trim.toInt)
  } catch {
    case e: NumberFormatException => ParseResult.error("Not int: $p")
  }

  def parseInt(p: String, default: Int): ParseResult[Int] = try {
    if (Strings.blankp(p))
      ParseResult.success(default)
    else
      ParseResult.success(p.trim.toInt)
  } catch {
    case e: NumberFormatException => ParseResult.error("Not int: $p")
  }

  def parseLong(p: Option[String]): ParseResult[Long] =
    p.map(parseLong).getOrElse(ParseResult.empty)

  def parseLong(p: String): ParseResult[Long] = try {
    if (Strings.blankp(p))
      ParseResult.empty
    else
      ParseResult.success(p.trim.toLong)
  } catch {
    case e: NumberFormatException => ParseResult.error("Not long: $p")
  }

  def parseDouble(p: Option[String]): ParseResult[Double] =
    p.map(parseDouble).getOrElse(ParseResult.empty)

  def parseDouble(p: String): ParseResult[Double] = try {
    if (Strings.blankp(p))
      ParseResult.empty
    else
      ParseResult.success(p.trim.toDouble)
  } catch {
    case e: NumberFormatException => ParseResult.error(s"Not double: $p")
  }

  def consequenceByte(p: Any): Consequence[Byte] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceByte(a)
    } yield b
    case m: Byte => Consequence.success(m)
    case m: Short => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toByte)
    case m: Int => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toByte)
    case m: Long => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toByte)
    case m: Float => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toByte
        if (r.toFloat == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: Double => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toByte
        if (r.toDouble == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: BigInt => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toByte)
    case m: BigDecimal => if (m > java.lang.Byte.MAX_VALUE || m < java.lang.Byte.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toByte
        if (BigDecimal(r) == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger =>
      val x = BigInt(m)
      if (x > java.lang.Byte.MAX_VALUE || x < java.lang.Byte.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence(x.toByte)
    case m: java.math.BigDecimal => 
      val x = BigDecimal(m)
      if (x > java.lang.Byte.MAX_VALUE || x < java.lang.Byte.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence.run {
          val r = x.toByte
          if (r.toFloat == m)
            Consequence.success(r)
          else
            Consequence.valueDomainFault(AnyUtils.toString(p))
        }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceShort(p: Any): Consequence[Short] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceShort(a)
    } yield b
    case m: Byte => Consequence.success(m.toShort)
    case m: Short => Consequence(m)
    case m: Int => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toShort)
    case m: Long => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toShort)
    case m: Float => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toShort
        if (r.toFloat == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: Double => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toShort
        if (r.toDouble == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: BigInt => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toShort)
    case m: BigDecimal => if (m > java.lang.Short.MAX_VALUE || m < java.lang.Short.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toShort
        if (BigDecimal(r) == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger =>
      val x = BigInt(m)
      if (x > java.lang.Short.MAX_VALUE || x < java.lang.Short.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence(x.toShort)
    case m: java.math.BigDecimal => 
      val x = BigDecimal(m)
      if (x > java.lang.Short.MAX_VALUE || x < java.lang.Short.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence.run {
          val r = x.toShort
          if (r.toFloat == m)
            Consequence.success(r)
          else
            Consequence.valueDomainFault(AnyUtils.toString(p))
        }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceInt(p: Any): Consequence[Int] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceInt(a)
    } yield b
    case m: Byte => Consequence.success(m.toInt)
    case m: Short => Consequence.success(m.toInt)
    case m: Int => Consequence.success(m)
    case m: Long => if (m > java.lang.Integer.MAX_VALUE || m < java.lang.Integer.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toInt)
    case m: Float => if (m > java.lang.Integer.MAX_VALUE || m < java.lang.Integer.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toInt
        if (r.toFloat == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: Double => if (m > java.lang.Integer.MAX_VALUE || m < java.lang.Integer.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toInt
        if (r.toDouble == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: BigInt => if (m > java.lang.Integer.MAX_VALUE || m < java.lang.Integer.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toInt)
    case m: BigDecimal => if (m > java.lang.Integer.MAX_VALUE || m < java.lang.Integer.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toInt
        if (BigDecimal(r) == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger =>
      val x = BigInt(m)
      if (x > java.lang.Integer.MAX_VALUE || x < java.lang.Integer.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence(x.toInt)
    case m: java.math.BigDecimal => 
      val x = BigDecimal(m)
      if (x > java.lang.Integer.MAX_VALUE || x < java.lang.Integer.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence.run {
          val r = x.toInt
          if (r.toFloat == m)
            Consequence.success(r)
          else
            Consequence.valueDomainFault(AnyUtils.toString(p))
        }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceLong(p: Any): Consequence[Long] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceLong(a)
    } yield b
    case m: Byte => Consequence.success(m.toLong)
    case m: Short => Consequence.success(m.toLong)
    case m: Int => Consequence.success(m.toLong)
    case m: Long => Consequence.success(m)
    case m: Float => if (m > java.lang.Long.MAX_VALUE || m < java.lang.Long.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toLong
        if (r.toFloat == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: Double => if (m > java.lang.Long.MAX_VALUE || m < java.lang.Long.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toLong
        if (r.toDouble == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: BigInt => if (m > java.lang.Long.MAX_VALUE || m < java.lang.Long.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence(m.toLong)
    case m: BigDecimal => if (m > java.lang.Long.MAX_VALUE || m < java.lang.Long.MIN_VALUE)
      Consequence.valueDomainFault(AnyUtils.toString(p))
    else
      Consequence.run {
        val r = m.toLong
        if (BigDecimal(r) == m)
          Consequence.success(r)
        else
          Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger =>
      val x = BigInt(m)
      if (x > java.lang.Long.MAX_VALUE || x < java.lang.Long.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence(x.toLong)
    case m: java.math.BigDecimal => 
      val x = BigDecimal(m)
      if (x > java.lang.Long.MAX_VALUE || x < java.lang.Long.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence.run {
          val r = x.toLong
          if (r.toFloat == m)
            Consequence.success(r)
          else
            Consequence.valueDomainFault(AnyUtils.toString(p))
        }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceFloat(p: Any): Consequence[Float] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceFloat(a)
    } yield b
    case m: Byte => Consequence.success(m.toFloat)
    case m: Short => Consequence.success(m.toFloat)
    case m: Int => Consequence.success(m.toFloat)
    case m: Long => Consequence.success(m.toFloat)
    case m: Float => Consequence.success(m)
    case m: Double => Consequence.run {
      val r = m.toFloat
      if (r.toDouble == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigInt => Consequence.run {
      val r = m.toFloat
      if (BigInt(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigDecimal => Consequence.run {
      val r = m.toFloat
      if (BigDecimal(r) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger => Consequence.run {
      val r = BigInt(m).toFloat
      if (new java.math.BigInteger(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: java.math.BigDecimal => Consequence.run {
      val r = BigDecimal(m).toFloat
      if (new java.math.BigDecimal(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceDouble(p: Any): Consequence[Double] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceDouble(a)
    } yield b
    case m: Byte => Consequence.success(m.toDouble)
    case m: Short => Consequence.success(m.toDouble)
    case m: Int => Consequence.success(m.toDouble)
    case m: Long => Consequence.success(m.toDouble)
    case m: Float => Consequence.success(m.toDouble)
    case m: Double => Consequence.success(m)
    case m: BigInt => Consequence.run {
      val r = m.toDouble
      if (BigInt(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigDecimal => Consequence.run {
      val r = m.toDouble
      if (BigDecimal(r) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
      }
    case m: java.math.BigInteger => Consequence.run {
      val r = BigInt(m).toDouble
      if (new java.math.BigInteger(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: java.math.BigDecimal => Consequence.run {
      val r = BigDecimal(m).toDouble
      if (new java.math.BigDecimal(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceBigInt(p: Any): Consequence[BigInt] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceBigInt(a)
    } yield b
    case m: Byte => Consequence.success(BigInt(m))
    case m: Short => Consequence.success(BigInt(m))
    case m: Int => Consequence.success(BigInt(m))
    case m: Long => Consequence.success(BigInt(m))
    case m: Float => Consequence.run {
      val r = BigInt(m.toString)
      if (r.toFloat == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: Double => Consequence.run {
      val r = BigInt(m.toString)
      if (r.toDouble == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigInt => Consequence.success(m)
    case m: BigDecimal => Consequence.run {
      val r = BigInt(m.toString)
      if (BigDecimal(r) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: java.math.BigInteger => Consequence(BigInt(m))
    case m: java.math.BigDecimal => 
      val x = BigDecimal(m)
      if (x > java.lang.Short.MAX_VALUE || x < java.lang.Short.MIN_VALUE)
        Consequence.valueDomainFault(AnyUtils.toString(p))
      else
        Consequence.run {
          val r = x.toShort
          if (r.toFloat == m)
            Consequence.success(r)
          else
            Consequence.valueDomainFault(AnyUtils.toString(p))
        }
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def consequenceBigDecimal(p: Any): Consequence[BigDecimal] = p match {
    case m: String => for {
      a <- Consequence.from(parse(m))
      b <- consequenceBigDecimal(a)
    } yield b
    case m: Byte => Consequence.success(BigDecimal(m))
    case m: Short => Consequence.success(BigDecimal(m))
    case m: Int => Consequence.success(BigDecimal(m))
    case m: Long => Consequence.success(BigDecimal(m))
    case m: Float => Consequence.success(m)
    case m: Double => Consequence.run {
      val r = BigDecimal(m)
      if (r.toDouble == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigInt => Consequence.run {
      val r = BigDecimal(m)
      if (BigInt(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: BigDecimal => Consequence.success(m)
    case m: java.math.BigInteger => Consequence.run {
      val r = BigInt(m).toFloat
      if (new java.math.BigInteger(r.toString) == m)
        Consequence.success(r)
      else
        Consequence.valueDomainFault(AnyUtils.toString(p))
    }
    case m: java.math.BigDecimal => Consequence(BigDecimal(m))
    case _ => Consequence.valueDomainFault(AnyUtils.toString(p))
  }

  def addScaleZero(lhs: BigDecimal, rhs: Float, mc: MathContext): BigDecimal = {
    val a = lhs.apply(mc) + rhs
    roundScaleZero(a, mc)
  }

  def addScale(lhs: BigDecimal, rhs: Float, mc: MathContext, scale: Int): BigDecimal = {
    val a = lhs.apply(mc) + rhs
    roundScale(a, mc, scale)
  }

  def subtractScaleZero(lhs: BigDecimal, rhs: Float, mc: MathContext): BigDecimal = {
    val a = lhs.apply(mc) - rhs
    roundScaleZero(a, mc)
  }

  def subtractScale(lhs: BigDecimal, rhs: Float, mc: MathContext, scale: Int): BigDecimal = {
    val a = lhs.apply(mc) - rhs
    roundScale(a, mc, scale)
  }

  def multiplyScaleZero(lhs: BigDecimal, rhs: Float, mc: MathContext): BigDecimal = {
    val a = lhs.apply(mc) * rhs
    roundScaleZero(a, mc)
  }

  def multiplyScaleZero(lhs: BigDecimal, rhs: BigDecimal, mc: MathContext): BigDecimal = {
    val a = lhs.apply(mc) * rhs
    roundScaleZero(a, mc)
  }

  def multiplyScale(lhs: BigDecimal, rhs: Float, mc: MathContext, scale: Int): BigDecimal = {
    val a = lhs.apply(mc) * rhs
    roundScale(a, mc, scale)
  }

  def divideScaleZero(lhs: BigDecimal, rhs: Float, mc: MathContext): BigDecimal = {
    val a = lhs.apply(mc) / rhs
    roundScaleZero(a, mc)
  }

  def divideScale(lhs: BigDecimal, rhs: Float, mc: MathContext, scale: Int): BigDecimal = {
    val a = lhs.apply(mc) / rhs
    roundScale(a, mc, scale)
  }

  def roundScaleZero(p: BigDecimal, mc: MathContext): BigDecimal =
    roundScaleZero(p, mc.getRoundingMode)

  def roundScaleZero(p: BigDecimal, rm: RoundingMode): BigDecimal =
    roundScale(p, rm, 0)

  def roundScaleZeroHalfUp(p: BigDecimal): BigDecimal =
    roundScaleHalfUp(p, 0)

  def roundScale(p: BigDecimal, mc: MathContext, scale: Int): BigDecimal =
    p.setScale(scale, roundingModeJavaToScala(mc.getRoundingMode)).apply(p.mc)

  def roundScale(p: BigDecimal, rm: RoundingMode, scale: Int): BigDecimal =
    p.setScale(scale, roundingModeJavaToScala(rm)).apply(p.mc)

  def roundScaleHalfUp(p: BigDecimal, scale: Int): BigDecimal =
    p.setScale(scale, BigDecimal.RoundingMode.HALF_UP).apply(p.mc)

  def roundingModeJavaToScala(p: RoundingMode): BigDecimal.RoundingMode.RoundingMode = p match {
      case RoundingMode.UP => BigDecimal.RoundingMode.UP
      case RoundingMode.DOWN => BigDecimal.RoundingMode.DOWN
      case RoundingMode.CEILING => BigDecimal.RoundingMode.CEILING // ceil(p)
      case RoundingMode.FLOOR => BigDecimal.RoundingMode.FLOOR // floor(p)
      case RoundingMode.HALF_UP => BigDecimal.RoundingMode.HALF_UP
      case RoundingMode.HALF_DOWN => BigDecimal.RoundingMode.HALF_DOWN
      case RoundingMode.HALF_EVEN => BigDecimal.RoundingMode.HALF_EVEN
      case RoundingMode.UNNECESSARY => BigDecimal.RoundingMode.UNNECESSARY
  }

  def roundingModeScalaToJava(p: BigDecimal.RoundingMode.RoundingMode): RoundingMode = p match {
      case BigDecimal.RoundingMode.UP => RoundingMode.UP
      case BigDecimal.RoundingMode.DOWN => RoundingMode.DOWN
      case BigDecimal.RoundingMode.CEILING => RoundingMode.CEILING // ceil(p)
      case BigDecimal.RoundingMode.FLOOR => RoundingMode.FLOOR // floor(p)
      case BigDecimal.RoundingMode.HALF_UP => RoundingMode.HALF_UP
      case BigDecimal.RoundingMode.HALF_DOWN => RoundingMode.HALF_DOWN
      case BigDecimal.RoundingMode.HALF_EVEN => RoundingMode.HALF_EVEN
      case BigDecimal.RoundingMode.UNNECESSARY => RoundingMode.UNNECESSARY
  }

  def round(p: BigDecimal): BigDecimal = BigDecimal(scala.math.round(p.doubleValue), p.mc)
  def floor(p: BigDecimal): BigDecimal = BigDecimal(scala.math.floor(p.doubleValue), p.mc)
  def ceil(p: BigDecimal): BigDecimal = BigDecimal(scala.math.ceil(p.doubleValue), p.mc)

  // handle BigDecimal("123.000")
  def normalizeBigDecimal(p: BigDecimal): BigDecimal =
    if (p.toString.contains('.') && p.isValidLong)
      BigDecimal(p.intValue)
    else
      p
}
