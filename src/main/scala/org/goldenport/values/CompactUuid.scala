package org.goldenport.values

import java.util.UUID
import java.math.BigInteger

/*
 * Derived from friendly_id (https://github.com/Devskiller/friendly-id)
 * 
 * @since   Dec. 26, 2020
 *  version Dec. 27, 2020
 * @version Nov.  4, 2022
 * @author  ASAMI, Tomoharu
 */
case class CompactUuid(uuid: UUID) {
  import CompactUuid._

  override def toString() = encode(uuid)
}

object CompactUuid {
  def apply(uuid: String): CompactUuid = CompactUuid(UUID.fromString(uuid))

  def generate(): CompactUuid = CompactUuid(UUID.randomUUID())

  def generateString(): String = encode(UUID.randomUUID())

  def encode(uuid: UUID): String = url62.encode(uuid)

  def encode(uuid: String): String = encode(UUID.fromString(uuid))

  def decode(cuuid: String): UUID = url62.decode(cuuid)

  object url62 {
    def encode(uuid: UUID): String = {
      val pair = UuidConverter.toBigInt(uuid)
      Base62.encode(pair)
    }

    def decode(cuuid: String): UUID = {
      val decoded = Base62.decode(cuuid)
      UuidConverter.toUuid(decoded)
    }
  }

  object UuidConverter {
    def toBigInt(uuid: UUID): BigInt =
      BigIntPair.pair(
        BigInt(uuid.getMostSignificantBits),
        BigInt(uuid.getLeastSignificantBits)
      )

    def toUuid(num: BigInt): UUID = {
      val (hi, lo) = BigIntPair.unpair(num)
      new UUID(hi.bigInteger.longValueExact, lo.bigInteger.longValueExact)
    }
  }

  object Base62 {
    val BASE = BigInt(62)
    val DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    def encode(num: BigInt): String = {
      require (num >= 0, "number nust not be negative")
      val b = new StringBuilder()
      var n: BigInteger = num.bigInteger
      while (n.compareTo(BigInteger.ZERO) > 0) {
        val divmod: Array[BigInteger] = n.divideAndRemainder(BASE.bigInteger)
        n = divmod(0)
        val d = divmod(1).intValue
        b.insert(0, DIGITS.charAt(d))
      }
      if (b.isEmpty)
        "0"
      else
        b.toString
    }

    def decode(s: String): BigInt = decode(s, 128)

    def decode(s: String, intLimit: Int): BigInt = {
      require (s != null, "Decoded string must not be null")
      require (s.length > 0, "String must not be empty")
      require (s.forall(x => DIGITS.contains(x)), s"String '${s}' contains illegal characters, only '$DIGITS' are allowed")
      var n = BigInt(0)
      for (i <- 0 until s.length) {
        val x = _char_at(s, i)
        n = n + (x * BASE.pow(i))
      }
      n
    }

    private def _char_at(p: String, i: Int): Int =
      DIGITS.indexOf(p.charAt(p.length - i - 1))
  }

  object BigIntPair {
    val HALF = BigInt(BigInteger.ONE.shiftLeft(64))
    val MAX_LONG = BigInt(java.lang.Long.MAX_VALUE)

    def pair(hi: BigInt, lo: BigInt): BigInt = {
      val ulo = toUnsigned(lo)
      val uhi = toUnsigned(hi)
      ulo + (uhi * HALF)
    }

    def unpair(p: BigInt): (BigInt, BigInt) = {
      val parts = p.bigInteger.divideAndRemainder(HALF.bigInteger)
      val shi = toSigned(parts(0))
      val slo = toSigned(parts(1))
      (shi, slo)
    }

    def toUnsigned(p: BigInt): BigInt =
      if (p.signum < 0)
        p + HALF
      else
        p

    def toSigned(p: BigInt): BigInt =
      if (MAX_LONG < p)
        p - HALF
      else
        p
  }
}
