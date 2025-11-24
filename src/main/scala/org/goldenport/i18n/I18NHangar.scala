package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale

/*
 * @since   Aug. 31, 2025
 *  version Sep. 10, 2025
 * @version Nov. 19, 2025
 * @author  ASAMI, Tomoharu
 */
case class I18NHangar[+T](
  map: Map[Locale, Vector[T]] = Map.empty[Locale, Vector[T]],
  commons: Vector[T] = Vector.empty
) {
  def get(locale: Locale): Option[Vector[T]] = map.get(locale)

  def valueVector: Vector[T] = commons ++ map.values.toVector.flatten // XXX

  def valueVectorLocale(locale: Locale): Vector[T] = commons ++ get(locale).getOrElse(Vector.empty)

  def valueVectorEn: Vector[T] = valueVectorLocale(LocaleUtils.en)

  def valueVectorJa: Vector[T] = valueVectorLocale(LocaleUtils.ja)

  def localeVectorMap: Map[Locale, Vector[T]] = map.mapValues(x => commons ++ x)

  def unify: Either[Vector[T], Map[Locale, Vector[T]]] =
    if (map.isEmpty)
      Left(commons)
    else
      Right(map.mapValues(x => commons ++ x))

  def add[U >: T](p: U): I18NHangar[U] =
    I18NHangar[U](
      map.asInstanceOf[Map[Locale, Vector[U]]],
      commons :+ p
    )

  def add[U >: T](locale: Locale, p: U): I18NHangar[U] = {
    val updated = map.get(locale) match {
      case Some(xs) => map + (locale -> (xs :+ p))
      case None     => map + (locale -> Vector(p))
    }
    I18NHangar[U](
      updated.asInstanceOf[Map[Locale, Vector[U]]],
      commons
    )
  }

  def filter(f: T => Boolean): I18NHangar[T] = I18NHangar(
    map.mapValues(_.filter(f)),
    commons.filter(f)
  )

  def filterNot(f: T => Boolean): I18NHangar[T] = I18NHangar(
    map.mapValues(_.filterNot(f)),
    commons.filterNot(f)
  )

  def map[U](f: T => U): I18NHangar[U] = mapValue(f)

  def mapValue[U](f: T => U): I18NHangar[U] =
    I18NHangar[U](map.mapValues(_.map(f)), commons.map(f))

  def mapValueCollection[U](f: Vector[T] => Vector[U]): I18NHangar[U] =
    I18NHangar[U](map.mapValues(f), f(commons))
}

object I18NHangar {
  private val _empty = I18NHangar()
  def empty[T] = _empty.asInstanceOf[I18NHangar[T]]

  case class Builder[T](
    map: Map[Locale, Vector[T]] = Map.empty[Locale, Vector[T]],
    common: Vector[T] = Vector.empty
  ) {
    def build(): I18NHangar[T] = I18NHangar(map, common)

    def add(p: Map[Locale, T]): Builder[T] = copy(map = map |+| p.mapValues(x => Vector(x)))

    def add(p: T): Builder[T] = copy(common = common :+ p)

    def add(p: I18NHangar[T]) = copy(
      map = map |+| p.map,
      common = common ++ p.commons
    )
  }

  def create[T](locale: Locale, p: T, ps: T*): I18NHangar[T] =
    create(Vector(locale -> (p +: ps)))

  def create[T](ps: Seq[(Locale, Seq[T])]): I18NHangar[T] = create(ps.toMap)

  def create[T](ps: Map[Locale, Seq[T]]): I18NHangar[T] = I18NHangar(ps.mapValues(_.toVector))

  def createOne[T](ps: Seq[(Locale, T)]): I18NHangar[T] =
    create(ps.map {
      case (k, v) => k -> Vector(v)
    })

  def createOne[T](ps: Map[Locale, T]): I18NHangar[T] = I18NHangar(ps.mapValues(x => Vector(x)))

  def createOne[T](
    ps: Map[Locale, T],
    c: T
  ): I18NHangar[T] = I18NHangar(ps.mapValues(x => Vector(x)), Vector(c))

  def createCommons[T](p: T): I18NHangar[T] = I18NHangar(commons = Vector(p))

  def createCommons[T](ps: Seq[T]): I18NHangar[T] = I18NHangar(commons = ps.toVector)
}
