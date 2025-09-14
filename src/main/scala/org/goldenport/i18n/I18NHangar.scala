package org.goldenport.i18n

import scalaz._, Scalaz._
import java.util.Locale

/*
 * @since   Aug. 31, 2025
 * @version Sep. 10, 2025
 * @author  ASAMI, Tomoharu
 */
case class I18NHangar[T](
  map: Map[Locale, Vector[T]] = Map.empty[Locale, Vector[T]],
  commons: Vector[T] = Vector.empty
) {
  def get(locale: Locale): Option[Vector[T]] = map.get(locale)

  def valueVector: Vector[T] = commons ++ map.values.toVector.flatten

  def valueVectorLocale(locale: Locale): Vector[T] = commons ++ get(locale).getOrElse(Vector.empty)

  def valueVectorEn: Vector[T] = valueVectorLocale(LocaleUtils.en)

  def valueVectorJa: Vector[T] = valueVectorLocale(LocaleUtils.ja)

  def unify: Either[Vector[T], Map[Locale, Vector[T]]] =
    if (map.isEmpty)
      Left(commons)
    else
      Right(map.mapValues(x => commons ++ x))

  def filterNot(f: T => Boolean): I18NHangar[T] = I18NHangar(
    map.mapValues(_.filterNot(f)),
    commons.filterNot(f)
  )

  def mapValueCollection(f: Vector[T] => Vector[T]): I18NHangar[T] =
    copy(map = map.mapValues(f))
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
