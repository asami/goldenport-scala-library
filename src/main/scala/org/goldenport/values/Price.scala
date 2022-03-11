package org.goldenport.values

import scalaz._, Scalaz._
import java.math.MathContext
import spire.math.Rational
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.hocon.HoconUtils

/*
 * @since   Apr. 12, 2018
 *  version Jun. 29, 2018
 *  version May. 18, 2019
 *  version Sep.  4, 2020
 *  version Oct. 26, 2020
 *  version Nov.  1, 2020
 *  version Dec. 20, 2020
 * @version Mar. 10, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait Price {
  def isZero: Boolean
  def displayPrice: BigDecimal
  def isTaxExclusive: Boolean
  def priceIncludingTax: BigDecimal
  def priceExcludingTax: BigDecimal
  def taxRational: Rational
  def mathContext: MathContext
  def toMap: Map[String, Any]
  def marshall: String = HoconUtils.makeJsonString(toMap)
  lazy val tax = taxRational.toBigDecimal(mathContext)

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
  val PROP_KIND = "kind"
  val PROP_PRICE = "price"
  val PROP_TAX = "tax"

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
  mathContext: MathContext = MathContext.DECIMAL32
) extends Price {
  import Price._

  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = true
  def priceIncludingTax = price + tax
  def priceExcludingTax = price
  def toIncludingTax: PriceIncludingTax = PriceIncludingTax(price + tax, tax)

  def +(rhs: PriceExcludingTax): PriceExcludingTax =
    PriceExcludingTax(price + rhs.price, tax + rhs.tax)
  def -(rhs: PriceExcludingTax): PriceExcludingTax =
    PriceExcludingTax(price - rhs.price, tax - rhs.tax)
  def *(rhs: Rational): PriceExcludingTax = PriceExcludingTax((price * rhs).toBigDecimal(mathContext), (tax * rhs).toBigDecimal(mathContext))

  // def +(rhs: BigDecimal): PriceExcludingTax = copy(price + rhs)
  // def -(rhs: BigDecimal): PriceExcludingTax = copy(price - rhs)

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
  taxRational: Rational,
  mathContext: MathContext = MathContext.DECIMAL32
) extends Price {
  import Price._

  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price - tax
  def toExcludingTax: PriceExcludingTax = PriceExcludingTax(price - tax, tax)

  def +(rhs: PriceIncludingTax): PriceIncludingTax = 
    PriceIncludingTax(price + rhs.price, tax + rhs.tax)
  def -(rhs: PriceIncludingTax): PriceIncludingTax = 
    PriceIncludingTax(price - rhs.price, tax - rhs.tax)
  def -(rhs: PriceNoTax): PriceIncludingTax =
    PriceIncludingTax(price - rhs.price, tax - rhs.tax)
  def *(rhs: Rational): PriceIncludingTax = PriceIncludingTax((price * rhs).toBigDecimal(mathContext), (tax * rhs).toBigDecimal(mathContext))

  // def +(rhs: BigDecimal): PriceIncludingTax = copy(price + rhs)
  // def -(rhs: BigDecimal): PriceIncludingTax = copy(price - rhs)

  def toMap = Map(
    PROP_KIND -> PriceIncludingTax.NAME,
    PROP_PRICE -> price,
    PROP_TAX -> taxRational
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
    val tax = (p / (1 + rate)) * rate
    PriceIncludingTax(p, tax)
  }

  def calcTaxRateRationalByPercent(percent: Int): Rational =
    percent * Rational(1, 100)

  def unmarshall(p: Hocon): Consequence[PriceIncludingTax] = for {
    price <- p.cBigDecimal(PROP_PRICE)
    tax <- p.cRational(PROP_TAX)
  } yield PriceIncludingTax(price, tax)
}

case class PriceNoTax(price: BigDecimal) extends Price {
  import Price._

  val mathContext: MathContext = MathContext.DECIMAL32
  def isZero = price == 0
  def displayPrice = price
  def isTaxExclusive = false
  def priceIncludingTax = price
  def priceExcludingTax = price
  def taxRational = 0
  def toIncludingTax: PriceIncludingTax = PriceIncludingTax(price, 0)
  def toExcludingTax: PriceExcludingTax = PriceExcludingTax(price, 0)

  def +(rhs: PriceNoTax): PriceNoTax =
    PriceNoTax(price + rhs.price)
  def -(rhs: PriceNoTax): PriceNoTax =
    PriceNoTax(price - rhs.price)

  def +(rhs: BigDecimal): PriceNoTax = copy(price - rhs)
  def -(rhs: BigDecimal): PriceNoTax = copy(price - rhs)

  def *(rhs: Rational): PriceNoTax = PriceNoTax((price * rhs).toBigDecimal(mathContext))

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
