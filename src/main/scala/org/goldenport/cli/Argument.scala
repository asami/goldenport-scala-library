package org.goldenport.cli

import java.net.URL
import java.net.URI
import org.goldenport.Strings
import org.goldenport.cli.spec.{Parameter => SpecParameter}
import org.goldenport.context.ConsequenceValueHelper
import org.goldenport.bag.BufferBag
import org.goldenport.util.AnyUtils

/*
 * @since   Oct.  5, 2018
 *  version May. 19, 2019
 *  version Feb. 16, 2020
 *  version Apr. 25, 2021
 * @version Jan. 30, 2022
 * @author  ASAMI, Tomoharu
 */
case class Argument(
  value: Any,
  spec: Option[SpecParameter]
) extends ConsequenceValueHelper {
  def asString: String = AnyUtils.toString(value)
  def asUrl: URL = AnyUtils.toUrl(value)
  def asUrlList: List[URL] = value match {
    case m: URL => List(m)
    case m: URI => List(m.toURL)
    case m =>
      val s = AnyUtils.toString(m)
      val xs = Strings.totokens(";")
      xs.map(AnyUtils.toUrl)
  }
  def toInputText: String = BufferBag.fromUri(value.toString).toText

  
}

object Argument {
  def apply(p: Any): Argument = Argument(p, None)
}
