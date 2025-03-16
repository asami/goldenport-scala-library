package org.goldenport.cli

import java.net.URL
import java.net.URI
import org.goldenport.Strings
import org.goldenport.cli.spec.{Parameter => SpecParameter}
import org.goldenport.bag.BufferBag
import org.goldenport.util.NumberUtils
import org.goldenport.util.AnyUtils

/*
 * @since   Mar.  1, 2025
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
trait Parameter {
  def name: String
  def value: Any
  def spec: Option[SpecParameter]

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

  def asIntOrString: Either[Int, String] = NumberUtils.optionInt(value) match {
    case Some(s) => Left(s)
    case None => Right(asString)
  }

  def toInputText: String = BufferBag.fromUri(value.toString).toText
}
