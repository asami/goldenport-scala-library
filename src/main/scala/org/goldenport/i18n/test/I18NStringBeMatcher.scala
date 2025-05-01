package org.goldenport.i18n.test

import scalaz._, Scalaz._
import org.scalatest._
import org.scalatest.matchers._
import org.goldenport.test._
import org.goldenport.test.MatchResultUtils.Implicits._
import org.goldenport.context.test.ConsequenceBeMatcher
import org.goldenport.i18n.I18NString

/*
 * @since   Dec.  3, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class I18NStringBeMatcher(expected: I18NString) extends BeMatcher[I18NString] with Matchers with MatchResultHelper {
  def apply(actual: I18NString): MatchResult = {
    val c = match_be("c", expected.c, actual.c)
    val en = match_be("en", expected.en, actual.en)
    val ja = match_be("ja", expected.ja, actual.ja)
    val map = match_be("map", expected.map, actual.map)
    Vector(c, en, ja, map).concatenate
  }
}

object I18NStringBeMatcher {
  trait Matchers {
    def consequence_i18nstring(p: I18NString) = ConsequenceBeMatcher(I18NStringBeMatcher(p))
  }

  trait MatchResultHelper extends MatchResultHelperBase {
    protected def be_i18nstring(expected: I18NString, actual: I18NString): MatchResult =
      I18NStringBeMatcher(expected)(actual)

    protected def be_i18nstring(expected: Option[I18NString], actual: Option[I18NString]): MatchResult =
      be_option[I18NString](be_i18nstring(_, _), expected, actual)

    protected def be_option[T](matcher: (T, T) => MatchResult, expected: Option[T], actual: Option[T]): MatchResult =
      MatchResultUtils.matchOption(matcher, expected, actual)
  }

  // def apply(expected: Option[I18NString])(p: Option[I18NString]): MatchResult =
  //   OptionBeMatcher[I18NString](I18NStringBeMatcher(_)(_))(expected)(p)
}
