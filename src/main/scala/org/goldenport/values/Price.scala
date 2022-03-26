package org.goldenport.values

import scalaz._, Scalaz._
import java.math.MathContext
import java.math.RoundingMode
import spire.math.Rational
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.hocon.HoconUtils
import org.goldenport.util.NumberUtils

/*
 * @since   Apr. 12, 2018
 *  version Jun. 29, 2018
 *  version May. 18, 2019
 *  version Sep.  4, 2020
 *  version Oct. 26, 2020
 *  version Nov.  1, 2020
 *  version Dec. 20, 2020
 * @version Mar. 26, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait Price {
  def isZero: Boolean
  def displayPrice: BigDecimal
  def isTaxExclusive: Boolean
  def priceIncludingTax: BigDecimal
  def priceExcludingTax: BigDecimal
  def taxRational: Rational
  def taxRate: BigDecimal
  def mathContext: MathContext
  def withPrice(p: BigDecimal): Price
  def toScaleZero: Price
  def toMap: Map[String, Any]
  def marshall: String = HoconUtils.makeJsonString(toMap)
  lazy val tax = taxRational.toBigDecimal(mathContext)
  lazy val taxScaleZero = NumberUtils.roundScaleZeroHalfUp(tax)

  // def -(p: BigDecimal): Price

  // def toMap: Map[String, BigDecimal] = Map.empty // TODO
    // protected def to_record(p: Price): Record = {
    //   displayprice = p match {
    //     case m: PriceExcludingTax => m.priceExcludingTax
    //     case m: PriceIncludingTax => m.priceIncludingTax
    //   }
    //   Record.dataApp(
    //     "displayPrice" -> displayPrice,
    //     "isTaxExclusive" -> p.isPriceExcluding,
    //     "tax" -> p.tax,
    //     "priceExcludingTax" -> p.priceExcludingTax,
    //     "priceIncludingTax" -> p.priceIncludingTax
    //   )
    // }
}
object Price {
  val mathContext = new MathContext(MathContext.DECIMAL32.getPrecision, RoundingMode.HALF_UP)
  val PROP_KIND = "kind"
  val PROP_PRICE = "price"
  val PROP_TAX = "tax"
  val PROP_RATE = "rate"

  def plus(lhs: Price, rhs: Price): Price = {
    lhs match {
      case m: PriceExcludingTax => rhs match {
        case mm: PriceExcludingTax => m + mm
        case mm: PriceIncludingTax => m + mm.toExcludingTax
        case mm: PriceNoTax => m + mm.toExcludingTax
      }
      case m: PriceIncludingTax => rhs match {
        case mm: PriceExcludingTax => m + mm.toIncludingTax
        case mm: PriceIncludingTax => m + mm
        case mm: PriceNoTax => m + mm.toIncludingTax
      }
      case m: PriceNoTax => rhs match {
        case mm: PriceExcludingTax => mm + m.toExcludingTax
        case mm: PriceIncludingTax => mm + m.toIncludingTax
        case mm: PriceNoTax => m + mm
      }
    }
  }

  def minus(lhs: Price, rhs: Price): Price = {
    lhs match {
      case m: PriceExcludingTax => rhs match {
        case mm: PriceExcludingTax => m - mm
        case mm: PriceIncludingTax => m - mm.toExcludingTax
        case mm: PriceNoTax => RAISE.noReachDefect("PriceExcludingTax minus PriceNoTax")
      }
      case m: PriceIncludingTax => rhs match {
        case mm: PriceExcludingTax => m - mm.toIncludingTax
        case mm: PriceIncludingTax => m - mm
        case mm: PriceNoTax => RAISE.noReachDefect("PriceIncludingTax minus PriceNoTax")
      }
      case m: PriceNoTax => rhs match {
        case mm: PriceExcludingTax => RAISE.noReachDefect("PriceNoTax minus PriceExcludingTax")
        case mm: PriceIncludingTax => RAISE.noReachDefect("PriceNoTax minus PriceExcludingTax")
        case mm: PriceNoTax => m - mm
      }
    }
  }

  def minusImplicit(percent: Percent)(lhs: Price, rhs: Price): Price = {
    def normalize(p: Price) = p match {
      case mm: PriceExcludingTax => mm
      case mm: PriceIncludingTax => mm
      case mm: PriceNoTax => PriceIncludingTax.createByPercent(mm.price, percent)
    }
    minus(normalize(lhs), normalize(rhs))
  }

  def minusForce(lhs: Price, rhs: Price): Price = {
    lhs match {
      case m: PriceExcludingTax => rhs match {
        case mm: PriceExcludingTax => m - mm
        case mm: PriceIncludingTax => m - mm.toExcludingTax
        case mm: PriceNoTax => m - mm.toExcludingTax
      }
      case m: PriceIncludingTax => rhs match {
        case mm: PriceExcludingTax => m - mm.toIncludingTax
        case mm: PriceIncludingTax => m - mm
        case mm: PriceNoTax => m - mm.toIncludingTax
      }
      case m: PriceNoTax => rhs match {
        case mm: PriceExcludingTax => m.toExcludingTax - mm
        case mm: PriceIncludingTax => m.toIncludingTax - mm
        case mm: PriceNoTax => m - mm
      }
    }
  }

  implicit object PriceExcludingTaxMonoid extends Monoid[PriceExcludingTax] {
    def zero = PriceExcludingTax.ZERO
    def append(l: PriceExcludingTax, r: => PriceExcludingTax): PriceExcludingTax = l + r
  }

  implicit object PriceIncludingTaxMonoid extends Monoid[PriceIncludingTax] {
    def zero = PriceIncludingTax.ZERO
    def append(l: PriceIncludingTax, r: => PriceIncludingTax): PriceIncludingTax = l + r
  }

  implicit object PriceNoTaxMonoid extends Monoid[PriceNoTax] {
    def zero = PriceNoTax.ZERO
    def append(l: PriceNoTax, r: => PriceNoTax): PriceNoTax = l + r
  }

  implicit object PriceMonoid extends Monoid[Price] {
    def zero = PriceNoTax.ZERO
    def append(l: Price, r: => Price): Price = plus(l, r)
  }

  def unmarshall(p: String): Consequence[Price] = for {
    x <- Consequence(ConfigFactory.parseString(p))
    r <- unmarshall(x)
  } yield r

  def unmarshall(p: Map[String, Any]): Consequence[Price] = unmarshall(HoconUtils.createHocon(p))

  def unmarshall(p: Hocon): Consequence[Price] = for {
    kind <- p.cString("kind")
    r <- kind match {
      case PriceIncludingTax.NAME => PriceIncludingTax.unmarshall(p)
      case PriceExcludingTax.NAME => PriceExcludingTax.unmarshall(p)
      case PriceNoTax.NAME => PriceNoTax.unmarshall(p)
    }
  } yield r
}

case class PriceExcludingTax(
  price: BigDecimal,
  taxRational: Rational,
  mathContext: MathContext = Price.mathContext
) extends Price {
  import Price._

  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = true
  def priceIncludingTax = price + tax
  def priceExcludingTax = price
  def taxRate = (taxRational / price).toBigDecimal(mathContext)
  def toIncludingTax: PriceIncludingTax = PriceIncludingTax(price + tax, tax)

  def +(rhs: PriceExcludingTax): PriceExcludingTax =
    PriceExcludingTax(price + rhs.price, tax + rhs.tax)
  def -(rhs: PriceExcludingTax): PriceExcludingTax =
    PriceExcludingTax(price - rhs.price, tax - rhs.tax)
  def *(rhs: Rational): PriceExcludingTax = PriceExcludingTax((price * rhs).toBigDecimal(mathContext), (tax * rhs).toBigDecimal(mathContext))

  // def +(rhs: BigDecimal): PriceExcludingTax = copy(price + rhs)
  // def -(rhs: BigDecimal): PriceExcludingTax = copy(price - rhs)

  def withPrice(p: BigDecimal): PriceExcludingTax = PriceExcludingTax.createByRate(p, taxRate)

  def toScaleZero: PriceExcludingTax = withPrice(NumberUtils.roundScaleZeroHalfUp(price))

  def toMap = Map(
    PROP_KIND -> PriceExcludingTax.NAME,
    PROP_PRICE -> price,
    PROP_TAX -> taxRational
  )
}
object PriceExcludingTax {
  import Price._

  val NAME = "price-excluding-tax"
  val ZERO = PriceExcludingTax(BigDecimal(0), 0)

  def createByPercent(p: BigDecimal, percent: Int): PriceExcludingTax =
    createByRate(p, percent * Rational(1, 100))

  def createByPercent(p: BigDecimal, percent: Percent): PriceExcludingTax =
    createByRate(p, percent.rate)

  def createByRate(p: BigDecimal, rate: Rational): PriceExcludingTax = 
    PriceExcludingTax(p, p * rate)

  def unmarshall(p: Hocon): Consequence[PriceExcludingTax] = for {
    price <- p.cBigDecimal(PROP_PRICE)
    tax <- p.cRational(PROP_TAX)
  } yield PriceExcludingTax(price, tax)
}

case class PriceIncludingTax(
  price: BigDecimal,
  // taxRational: Rational,
  taxRateRational: Rational,
  mathContext: MathContext = Price.mathContext
) extends Price {
  import Price._

  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price - tax
  def taxRate: BigDecimal = taxRateRational.toBigDecimal(mathContext)
  def taxRational: Rational = (price / (1 + taxRateRational)) * taxRateRational
  // def taxRate = {
  //   val n = (taxRational / price).toBigDecimal(mathContext)
  //   NumberUtils.roundScaleHalfUp(n, 1)
  // }
  def toExcludingTax: PriceExcludingTax = PriceExcludingTax(price - tax, tax)

  def +(rhs: PriceIncludingTax): PriceIncludingTax =
    if (taxRateRational == rhs.taxRateRational)
      withPrice(price + rhs.price)
    else
      RAISE.noReachDefect("PriceIncludingTax unmatch tax rate")

  def -(rhs: PriceIncludingTax): PriceIncludingTax =
    if (taxRateRational == rhs.taxRateRational)
      withPrice(price - rhs.price)
    else
      RAISE.noReachDefect("PriceIncludingTax unmatch tax rate")

  def -(rhs: PriceNoTax): PriceIncludingTax = withPrice(price - rhs.price)

  def *(rhs: Rational): PriceIncludingTax = {
    withPrice((price * rhs).toBigDecimal(mathContext))
  }

  // def +(rhs: BigDecimal): PriceIncludingTax = copy(price + rhs)
  // def -(rhs: BigDecimal): PriceIncludingTax = copy(price - rhs)

  def withPrice(p: BigDecimal): PriceIncludingTax =
    PriceIncludingTax.createByRate(p, taxRate)

  def toScaleZero: PriceIncludingTax = withPrice(NumberUtils.roundScaleZeroHalfUp(price))

  def toMap = Map(
    PROP_KIND -> PriceIncludingTax.NAME,
    PROP_PRICE -> price,
    PROP_TAX -> taxRational,
    PROP_RATE -> taxRateRational
  )
}
object PriceIncludingTax {
  import Price._

  val NAME = "price-including-tax"
  val ZERO = PriceIncludingTax(BigDecimal(0), 0)

  def createByPercent(p: BigDecimal, percent: Int): PriceIncludingTax =
    createByRate(p, calcTaxRateRationalByPercent(percent))

  def createByPercent(p: BigDecimal, percent: Percent): PriceIncludingTax =
    createByRate(p, percent.rate)

  def createByRate(p: BigDecimal, rate: Rational): PriceIncludingTax = {
    // val price = p / (1 + rate)
    // println(s"price: $price")
    // val tax = p - price
    // println(s"tax: ${tax.toBigDecimal(mathContext)}")
    // val tax = (p / (1 + rate)) * rate
    // println(s"tax2: ${tax2.toBigDecimal(mathContext)}")
    PriceIncludingTax(p, rate)
  }

  def createByTax(p: BigDecimal, tax: Rational): PriceIncludingTax = {
    val rate = tax / (p - tax)
    val r = NumberUtils.roundScaleHalfUp(rate.toBigDecimal(mathContext), 2)
    PriceIncludingTax(p, Rational(r))
  }

  def calcTaxRateRationalByPercent(percent: Int): Rational =
    percent * Rational(1, 100)

  def unmarshall(p: Hocon): Consequence[PriceIncludingTax] = for {
    price <- p.cBigDecimal(PROP_PRICE)
    tax <- p.cRational(PROP_TAX) // old
    rate <- p.cRationalOption(PROP_RATE) // new
  } yield rate match {
    case Some(s) => PriceIncludingTax(price, s)
    case None => PriceIncludingTax.createByTax(price, tax)
  }
}

case class PriceNoTax(price: BigDecimal) extends Price {
  import Price._

  val mathContext: MathContext = Price.mathContext
  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price
  def taxRational = 0
  def taxRate = 0
  def toIncludingTax: PriceIncludingTax = PriceIncludingTax(price, 0)
  def toExcludingTax: PriceExcludingTax = PriceExcludingTax(price, 0)
  def toIncludingTax(rate: Rational): PriceIncludingTax = PriceIncludingTax.createByRate(price, rate)
  def toExcludingTax(rate: Rational): PriceExcludingTax = PriceExcludingTax.createByRate(price, rate)

  def +(rhs: PriceNoTax): PriceNoTax =
    PriceNoTax(price + rhs.price)
  def -(rhs: PriceNoTax): PriceNoTax =
    PriceNoTax(price - rhs.price)

  def +(rhs: BigDecimal): PriceNoTax = copy(price - rhs)
  def -(rhs: BigDecimal): PriceNoTax = copy(price - rhs)

  def *(rhs: Rational): PriceNoTax = PriceNoTax((price * rhs).toBigDecimal(mathContext))

  def withPrice(p: BigDecimal): PriceNoTax = PriceNoTax(p)

  def toScaleZero: PriceNoTax = PriceNoTax(NumberUtils.roundScaleZeroHalfUp(price))

  def toMap = Map(
    PROP_KIND -> PriceNoTax.NAME,
    PROP_PRICE -> price
  )
}
object PriceNoTax {
  import Price._

  val NAME = "price-no-tax"
  val ZERO = PriceNoTax(BigDecimal(0))

  def unmarshall(p: Hocon): Consequence[PriceNoTax] = for {
    price <- p.cBigDecimal(PROP_PRICE)
  } yield PriceNoTax(price)
}
