package org.goldenport.values

import java.util.Currency
import org.goldenport.util.CurrencyUtils._
import org.goldenport.util.NumberUtils

/*
 * @since   Sep. 25, 2019
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
case class Money(currency: Currency, amount: BigDecimal) {
}

object Money {
  def dollar(p: Number) = Money(Dollar, NumberUtils.toBigDecimal(p))
  def dollar(p: Int) = Money(Dollar, BigDecimal(p))
  def yen(p: Number) = Money(Yen, NumberUtils.toBigDecimal(p))
  def yen(p: Int) = Money(Yen, BigDecimal(p))
  def euro(p: Int) = Money(Euro, BigDecimal(p))
  def euro(p: Number) = Money(Euro, NumberUtils.toBigDecimal(p))
  def pound(p: Int) = Money(Pound, BigDecimal(p))
  def pound(p: Number) = Money(Pound, NumberUtils.toBigDecimal(p))
  def swissfranc(p: Int) = Money(SwissFranc, BigDecimal(p))
  def swissfranc(p: Number) = Money(SwissFranc, NumberUtils.toBigDecimal(p))

  def create(name: String, amount: Int): Money = {
    Option(Currency.getInstance(name)) match {
      case Some(s) => Money(s, BigDecimal(amount))
      case None => throw new IllegalArgumentException(s"Invalid currency: $name")
    }
  }
}
