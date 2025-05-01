package org.goldenport.util

import java.util.Currency

/*
 * @since   Oct.  9, 2024
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
object CurrencyUtils {
  lazy val Dollar = Currency.getInstance("USD")
  lazy val Yen = Currency.getInstance("JPY")
  lazy val Pound = Currency.getInstance("GBP")
  lazy val Euro = Currency.getInstance("EUR")
  lazy val SwissFranc = Currency.getInstance("CHF")

  val SYMBOL_DOLLAR = '$'
  val SYMBOL_DOLLAR_STRING = "$"
  val SYMBOL_YEN = '¥'
  val SYMBOL_YEN_STRING = "¥"
  val SYMBOL_POUND = '£'
  val SYMBOL_POUND_STRING = "£"
  val SYMBOL_EURO = '€'
  val SYMBOL_EURO_STRING = "€"
}
