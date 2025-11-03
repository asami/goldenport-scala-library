package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.typeclass.Zero

/*
 * @since   Jun. 24, 2025
 *  version Jun. 26, 2025
 *  version Jul. 27, 2025
 *  version Aug.  7, 2025
 *  version Sep.  6, 2025
 *  version Oct.  4, 2025
 * @version Nov.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class I18NContainer[+T](
  c: T, // special locale for programming language
  en: T, // default
  ja: T,
  map: Map[Locale, T]
) {
  def get(locale: Locale): Option[T] = {
    val l = Option(locale.getLanguage)
    val country = Option(locale.getCountry)
    val variant = Option(locale.getVariant)
    l flatMap { lang =>
      map.get(locale) orElse {
        // See com.asamioffice.goldenport.util.ULocale.match
        map.find(_._1.getLanguage === lang).map(_._2)
      }.orElse {
        if (lang === Locale.JAPANESE.getLanguage)
          Some(ja)
        else if (lang === Locale.ENGLISH.getLanguage)
          Some(en)
        else
          None
      }
    }
  }

  def localeVector: Vector[(Locale, T)] =
    if (c == en && en == ja)
      _locale_vector_without_c
    else if (c != en && en == ja)
      _locale_vector_with_c
    else if (c == en && en != ja)
      _locale_vector_without_c
    else
      _locale_vector_with_c

  private def _locale_vector_with_c = {
    val a = Map(LocaleUtils.C -> c, LocaleUtils.en -> en, LocaleUtils.ja -> ja)
    (map ++ a).toVector
  }

  private def _locale_vector_without_c = {
    val a = Map(LocaleUtils.en -> en, LocaleUtils.ja -> ja)
    (map ++ a).toVector
  }

  def isSimple: Boolean = c == en && en == ja && map.isEmpty

  def getIfNoLocale: Option[T] =
    if (isSimple)
      Some(c)
    else
      None

  def apply(locale: Locale): T = get(locale) getOrElse c

  def default = c

  def mapValues[A: Zero](f: T => A): I18NContainer[A] = {
    val a = localeVector.map {
      case (locale, xs) =>
        val x = f(xs)
        (locale -> x)
    }
    I18NContainer.create(a)
  }
}

object I18NContainer {
  def make[T](p: T): I18NContainer[T] = I18NContainer(p, p, p, Map.empty)

  def create[T: Zero](p: Seq[(Locale, T)]): I18NContainer[T] =
    create(p.toMap)

  def create[T: Zero](p: Map[Locale, T]): I18NContainer[T] = {
    val copt = p.get(LocaleUtils.C)
    val enopt = p.get(LocaleUtils.en)
    val jaopt = p.get(LocaleUtils.ja)
    val (c, e, j): (T, T, T) = (copt, enopt, jaopt) match {
      case (Some(c), Some(e), Some(j)) => (c, e, j)
      case (Some(c), Some(e), None) => (c, e, e)
      case (Some(c), None, Some(j)) => (c, j, j)
      case (Some(c), None, None) => (c, c, c)
      case (None, Some(e), Some(j)) => (e, e, j)
      case (None, Some(e), None) => (e, e, e)
      case (None, None, Some(j)) => (j, j, j)
      case (None, None, None) =>
        p.headOption match {
          case Some((_, s)) => (s, s, s)
          case None => 
            val empty = Zero[T].zero
            (empty, empty, empty)
        }
    }
    val a = p -- Set(LocaleUtils.C, LocaleUtils.en, LocaleUtils.ja)
    I18NContainer(c, e, j, a)
  }

  def createSeq[T](p: Map[Locale, Seq[T]]): I18NContainer[List[T]] =
    _create(p.mapValues(_.toList))

  def createSeq[T](p: Seq[(Locale, Seq[T])]): I18NContainer[List[T]] =
    createSeq(p.toMap)

  private def _create[T](p: Map[Locale, List[T]]): I18NContainer[List[T]] = {
    val copt = p.get(LocaleUtils.C)
    val enopt = p.get(LocaleUtils.en)
    val jaopt = p.get(LocaleUtils.ja)
    val (c, e, j) = (copt, enopt, jaopt) match {
      case (Some(c), Some(e), Some(j)) => (c, e, j)
      case (Some(c), Some(e), None) => (c, e, e)
      case (Some(c), None, Some(j)) => (c, j, j)
      case (Some(c), None, None) => (c, c, c)
      case (None, Some(e), Some(j)) => (e, e, j)
      case (None, Some(e), None) => (e, e, e)
      case (None, None, Some(j)) => (j, j, j)
      case (None, None, None) => (Nil, Nil, Nil)
    }
    val a = p -- Set(LocaleUtils.C, LocaleUtils.en, LocaleUtils.ja)
    I18NContainer(c, e, j, a)
  }

  def createList[T](p: I18NHangar[T]): I18NContainer[List[T]] = {
    val c = p.commons.toList
    val en = p.valueVectorEn.toList
    val ja = p.valueVectorJa.toList
    val others = p.map.map { case (locale, xs) => locale -> xs.toList }
    I18NContainer(c, en, ja, others)
  }

  def createVector[T](p: I18NHangar[T]): I18NContainer[Vector[T]] = {
    val c = p.commons
    val en = p.valueVectorEn
    val ja = p.valueVectorJa
    val others = p.map.map { case (locale, xs) => locale -> xs }
    I18NContainer(c, en, ja, others)
  }

  def createOneC[T: Zero](p: I18NHangar[T]): Consequence[I18NContainer[T]] = {
    def _head_opt_(xs: Vector[T]): Option[T] =
      if (xs.nonEmpty) Some(xs.head) else None

    val copt = _head_opt_(p.commons)
    val enopt = _head_opt_(p.valueVectorEn)
    val jaopt = _head_opt_(p.valueVectorJa)
    val others: Map[Locale, Option[T]] = p.map.mapValues(_head_opt_).toMap

    val multiLocales = p.map.collect {
      case (locale, xs) if xs.size > 1 => locale
    }.toVector

    if (multiLocales.nonEmpty)
      Consequence.noReachDefect(s"""I18NHangar contains multiple elements for locales: ${multiLocales.mkString(", ")}""")
    else {
      val c = copt orElse enopt orElse jaopt getOrElse Zero[T].zero
      val en = enopt.getOrElse(c)
      val ja = jaopt.getOrElse(c)
      val otherValues = others.collect { case (k, Some(v)) => k -> v }
      Consequence.success(I18NContainer(c, en, ja, otherValues))
    }
  }

  def enja[T](en: T, ja: T): I18NContainer[T] = I18NContainer(
    en, en, ja, Map.empty
  )
}
