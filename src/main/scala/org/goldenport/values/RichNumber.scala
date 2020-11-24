package org.goldenport.values

import scala.language.implicitConversions
import org.goldenport.util.AnyRefUtils

/*
 * @since   Sep. 28, 2020
 * @version Sep. 28, 2020
 * @author  ASAMI, Tomoharu
 */
case class RichNumber(underlying: Number) extends AnyVal {
  def +(p: Number): Number = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l + r
      case r: java.lang.Short => l + r
      case r: java.lang.Integer => l + r
      case r: java.lang.Long => l + r
      case r: java.lang.Float => l + r
      case r: java.lang.Double => l + r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).add(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Short => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Integer => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Long => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Float => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Double => l.add(java.math.BigInteger.valueOf(r.toLong))
      case r: java.math.BigInteger => l.add(r)
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).add(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) + r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) + r
      case r => _add(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.add(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Short => l.add(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Integer => l.add(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Long => l.add(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Float => l.add(java.math.BigDecimal.valueOf(r.toDouble))
      case r: java.lang.Double => l.add(java.math.BigDecimal.valueOf(r))
      case r: java.math.BigInteger => l.add(new java.math.BigDecimal(r.toString))
      case r: java.math.BigDecimal => l.add(r)
      case r: scala.math.BigInt => _add(l, r)
      case r: scala.math.BigDecimal => _add(l, r)
      case r => _add(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l + scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l + scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l + scala.math.BigInt(r)
      case r: java.lang.Long => l + scala.math.BigInt(r)
      case r: java.lang.Float => _add(l, r)
      case r: java.lang.Double => _add(l, r)
      case r: java.math.BigInteger => l + scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _add(l, r)
      case r: scala.math.BigInt => l + r
      case r: scala.math.BigDecimal => _add(l, r)
      case r => _add(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l + scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l + scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l + scala.math.BigDecimal(r)
      case r: java.lang.Long => l + scala.math.BigDecimal(r)
      case r: java.lang.Float => l + scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l + scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l + scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l + r
      case r: scala.math.BigInt => _add(l, r)
      case r: scala.math.BigDecimal => _add(l, r)
      case r => _add(l, r)
    }
    case l => _add(l, p)
  }

  private def _add(l: Any, r: Any): BigDecimal = 
    AnyRefUtils.toBigDecimal(l) + AnyRefUtils.toBigDecimal(r)

  def -(p: Number): Number = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l - r
      case r: java.lang.Short => l - r
      case r: java.lang.Integer => l - r
      case r: java.lang.Long => l - r
      case r: java.lang.Float => l - r
      case r: java.lang.Double => l - r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).subtract(r)
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Short => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Integer => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Long => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Float => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.lang.Double => l.subtract(java.math.BigInteger.valueOf(r.toLong))
      case r: java.math.BigInteger => l.subtract(r)
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).subtract(r)
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) - r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) - r
      case r => _sub(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.subtract(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Short => l.subtract(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Integer => l.subtract(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Long => l.subtract(java.math.BigDecimal.valueOf(r.toLong))
      case r: java.lang.Float => l.subtract(java.math.BigDecimal.valueOf(r.toDouble))
      case r: java.lang.Double => l.subtract(java.math.BigDecimal.valueOf(r))
      case r: java.math.BigInteger => l.subtract(new java.math.BigDecimal(r.toString))
      case r: java.math.BigDecimal => l.subtract(r)
      case r: scala.math.BigInt => _sub(l, r)
      case r: scala.math.BigDecimal => _sub(l, r)
      case r => _sub(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l - scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l - scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l - scala.math.BigInt(r)
      case r: java.lang.Long => l - scala.math.BigInt(r)
      case r: java.lang.Float => _sub(l, r)
      case r: java.lang.Double => _sub(l, r)
      case r: java.math.BigInteger => l - scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _sub(l, r)
      case r: scala.math.BigInt => l - r
      case r: scala.math.BigDecimal => _sub(l, r)
      case r => _sub(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l - scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l - scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l - scala.math.BigDecimal(r)
      case r: java.lang.Long => l - scala.math.BigDecimal(r)
      case r: java.lang.Float => l - scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l - scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l - scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l - r
      case r: scala.math.BigInt => _sub(l, r)
      case r: scala.math.BigDecimal => _sub(l, r)
      case r => _sub(l, r)
    }
    case l => _sub(l, p)
  }

  private def _sub(l: Any, r: Any): BigDecimal = 
    AnyRefUtils.toBigDecimal(l) - AnyRefUtils.toBigDecimal(r)

  def >(p: Number): Boolean = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l > r
      case r: java.lang.Short => l > r
      case r: java.lang.Integer => l > r
      case r: java.lang.Long => l > r
      case r: java.lang.Float => l > r
      case r: java.lang.Double => l > r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) > 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.lang.Short => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.lang.Integer => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.lang.Long => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.lang.Float => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.lang.Double => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) > 0
      case r: java.math.BigInteger => l.compareTo(r) > 0
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).compareTo(r) > 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) > r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) > r
      case r => _greater(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) > 0
      case r: java.lang.Short => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) > 0
      case r: java.lang.Integer => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) > 0
      case r: java.lang.Long => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) > 0
      case r: java.lang.Float => l.compareTo(java.math.BigDecimal.valueOf(r.toDouble)) > 0
      case r: java.lang.Double => l.compareTo(java.math.BigDecimal.valueOf(r)) > 0
      case r: java.math.BigInteger => l.compareTo(new java.math.BigDecimal(r.toString)) > 0
      case r: java.math.BigDecimal => l.compareTo(r) > 0
      case r: scala.math.BigInt => _greater(l, r)
      case r: scala.math.BigDecimal => _greater(l, r)
      case r => _greater(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l > scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l > scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l > scala.math.BigInt(r)
      case r: java.lang.Long => l > scala.math.BigInt(r)
      case r: java.lang.Float => _greater(l, r)
      case r: java.lang.Double => _greater(l, r)
      case r: java.math.BigInteger => l > scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _greater(l, r)
      case r: scala.math.BigInt => l > r
      case r: scala.math.BigDecimal => _greater(l, r)
      case r => _greater(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l > scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l > scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l > scala.math.BigDecimal(r)
      case r: java.lang.Long => l > scala.math.BigDecimal(r)
      case r: java.lang.Float => l > scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l > scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l > scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l > r
      case r: scala.math.BigInt => _greater(l, r)
      case r: scala.math.BigDecimal => _greater(l, r)
      case r => _greater(l, r)
    }
    case l => _greater(l, p)
  }

  private def _greater(l: Any, r: Any): Boolean = 
    AnyRefUtils.toBigDecimal(l) > AnyRefUtils.toBigDecimal(r)

  def >=(p: Number): Boolean = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l >= r
      case r: java.lang.Short => l >= r
      case r: java.lang.Integer => l >= r
      case r: java.lang.Long => l >= r
      case r: java.lang.Float => l >= r
      case r: java.lang.Double => l >= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) >= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.lang.Short => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.lang.Integer => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.lang.Long => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.lang.Float => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.lang.Double => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) >= 0
      case r: java.math.BigInteger => l.compareTo(r) >= 0
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).compareTo(r) >= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) >= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) >= r
      case r => _greater_equal(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) >= 0
      case r: java.lang.Short => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) >= 0
      case r: java.lang.Integer => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) >= 0
      case r: java.lang.Long => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) >= 0
      case r: java.lang.Float => l.compareTo(java.math.BigDecimal.valueOf(r.toDouble)) >= 0
      case r: java.lang.Double => l.compareTo(java.math.BigDecimal.valueOf(r)) >= 0
      case r: java.math.BigInteger => l.compareTo(new java.math.BigDecimal(r.toString)) >= 0
      case r: java.math.BigDecimal => l.compareTo(r) >= 0
      case r: scala.math.BigInt => _greater_equal(l, r)
      case r: scala.math.BigDecimal => _greater_equal(l, r)
      case r => _greater_equal(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l >= scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l >= scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l >= scala.math.BigInt(r)
      case r: java.lang.Long => l >= scala.math.BigInt(r)
      case r: java.lang.Float => _greater_equal(l, r)
      case r: java.lang.Double => _greater_equal(l, r)
      case r: java.math.BigInteger => l >= scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _greater_equal(l, r)
      case r: scala.math.BigInt => l >= r
      case r: scala.math.BigDecimal => _greater_equal(l, r)
      case r => _greater_equal(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l >= scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l >= scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l >= scala.math.BigDecimal(r)
      case r: java.lang.Long => l >= scala.math.BigDecimal(r)
      case r: java.lang.Float => l >= scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l >= scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l >= scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l >= r
      case r: scala.math.BigInt => _greater_equal(l, r)
      case r: scala.math.BigDecimal => _greater_equal(l, r)
      case r => _greater_equal(l, r)
    }
    case l => _greater_equal(l, p)
  }

  private def _greater_equal(l: Any, r: Any): Boolean = 
    AnyRefUtils.toBigDecimal(l) >= AnyRefUtils.toBigDecimal(r)

  def <(p: Number): Boolean = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l < r
      case r: java.lang.Short => l < r
      case r: java.lang.Integer => l < r
      case r: java.lang.Long => l < r
      case r: java.lang.Float => l < r
      case r: java.lang.Double => l < r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) < 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.lang.Short => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.lang.Integer => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.lang.Long => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.lang.Float => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.lang.Double => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) < 0
      case r: java.math.BigInteger => l.compareTo(r) < 0
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).compareTo(r) < 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) < r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) < r
      case r => _lesser(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) < 0
      case r: java.lang.Short => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) < 0
      case r: java.lang.Integer => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) < 0
      case r: java.lang.Long => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) < 0
      case r: java.lang.Float => l.compareTo(java.math.BigDecimal.valueOf(r.toDouble)) < 0
      case r: java.lang.Double => l.compareTo(java.math.BigDecimal.valueOf(r)) < 0
      case r: java.math.BigInteger => l.compareTo(new java.math.BigDecimal(r.toString)) < 0
      case r: java.math.BigDecimal => l.compareTo(r) < 0
      case r: scala.math.BigInt => _lesser(l, r)
      case r: scala.math.BigDecimal => _lesser(l, r)
      case r => _lesser(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l < scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l < scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l < scala.math.BigInt(r)
      case r: java.lang.Long => l < scala.math.BigInt(r)
      case r: java.lang.Float => _lesser(l, r)
      case r: java.lang.Double => _lesser(l, r)
      case r: java.math.BigInteger => l < scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _lesser(l, r)
      case r: scala.math.BigInt => l < r
      case r: scala.math.BigDecimal => _lesser(l, r)
      case r => _lesser(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l < scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l < scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l < scala.math.BigDecimal(r)
      case r: java.lang.Long => l < scala.math.BigDecimal(r)
      case r: java.lang.Float => l < scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l < scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l < scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l < r
      case r: scala.math.BigInt => _lesser(l, r)
      case r: scala.math.BigDecimal => _lesser(l, r)
      case r => _lesser(l, r)
    }
    case l => _lesser(l, p)
  }

  private def _lesser(l: Any, r: Any): Boolean = 
    AnyRefUtils.toBigDecimal(l) < AnyRefUtils.toBigDecimal(r)

  def <=(p: Number): Boolean = underlying match {
    case l: java.lang.Byte => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.lang.Short => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.lang.Integer => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.lang.Long => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.lang.Float => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.lang.Double => p match {
      case r: java.lang.Byte => l <= r
      case r: java.lang.Short => l <= r
      case r: java.lang.Integer => l <= r
      case r: java.lang.Long => l <= r
      case r: java.lang.Float => l <= r
      case r: java.lang.Double => l <= r
      case r: java.math.BigInteger => java.math.BigInteger.valueOf(l.toLong).compareTo(r) <= 0
      case r: java.math.BigDecimal => java.math.BigDecimal.valueOf(l.toLong).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.math.BigInteger => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.lang.Short => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.lang.Integer => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.lang.Long => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.lang.Float => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.lang.Double => l.compareTo(java.math.BigInteger.valueOf(r.toLong)) <= 0
      case r: java.math.BigInteger => l.compareTo(r) <= 0
      case r: java.math.BigDecimal => new java.math.BigDecimal(l.toString).compareTo(r) <= 0
      case r: scala.math.BigInt => AnyRefUtils.toBigInt(l) <= r
      case r: scala.math.BigDecimal => AnyRefUtils.toBigDecimal(l) <= r
      case r => _lesser_equal(l, r)
    }
    case l: java.math.BigDecimal => p match {
      case r: java.lang.Byte => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) <= 0
      case r: java.lang.Short => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) <= 0
      case r: java.lang.Integer => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) <= 0
      case r: java.lang.Long => l.compareTo(java.math.BigDecimal.valueOf(r.toLong)) <= 0
      case r: java.lang.Float => l.compareTo(java.math.BigDecimal.valueOf(r.toDouble)) <= 0
      case r: java.lang.Double => l.compareTo(java.math.BigDecimal.valueOf(r)) <= 0
      case r: java.math.BigInteger => l.compareTo(new java.math.BigDecimal(r.toString)) <= 0
      case r: java.math.BigDecimal => l.compareTo(r) <= 0
      case r: scala.math.BigInt => _lesser_equal(l, r)
      case r: scala.math.BigDecimal => _lesser_equal(l, r)
      case r => _lesser_equal(l, r)
    }
    case l: scala.math.BigInt => p match {
      case r: java.lang.Byte => l <= scala.math.BigInt(r.toInt)
      case r: java.lang.Short => l <= scala.math.BigInt(r.toInt)
      case r: java.lang.Integer => l <= scala.math.BigInt(r)
      case r: java.lang.Long => l <= scala.math.BigInt(r)
      case r: java.lang.Float => _lesser_equal(l, r)
      case r: java.lang.Double => _lesser_equal(l, r)
      case r: java.math.BigInteger => l <= scala.math.BigInt(r.toString)
      case r: java.math.BigDecimal => _lesser_equal(l, r)
      case r: scala.math.BigInt => l <= r
      case r: scala.math.BigDecimal => _lesser_equal(l, r)
      case r => _lesser_equal(l, r)
    }
    case l: scala.math.BigDecimal => p match {
      case r: java.lang.Byte => l <= scala.math.BigDecimal(r.toInt)
      case r: java.lang.Short => l <= scala.math.BigDecimal(r.toInt)
      case r: java.lang.Integer => l <= scala.math.BigDecimal(r)
      case r: java.lang.Long => l <= scala.math.BigDecimal(r)
      case r: java.lang.Float => l <= scala.math.BigDecimal(r.toDouble)
      case r: java.lang.Double => l <= scala.math.BigDecimal(r)
      case r: java.math.BigInteger => l <= scala.math.BigDecimal(r)
      case r: java.math.BigDecimal => l <= r
      case r: scala.math.BigInt => _lesser_equal(l, r)
      case r: scala.math.BigDecimal => _lesser_equal(l, r)
      case r => _lesser_equal(l, r)
    }
    case l => _lesser_equal(l, p)
  }

  private def _lesser_equal(l: Any, r: Any): Boolean = 
    AnyRefUtils.toBigDecimal(l) <= AnyRefUtils.toBigDecimal(r)
}

object RichNumber {
  object Implicits {
    implicit def enrichNumber(p: Number) = new RichNumber(p)
  }
}
