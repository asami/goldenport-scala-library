package org.goldenport.values

import java.util.Locale
import org.goldenport.i18n.I18NString

/*
 * @since   May.  2, 2020
 *  version Jun. 15, 2020
 * @version Oct. 14, 2023
 * @author  ASAMI, Tomoharu
 */
case class Designation(
  nameI18N: I18NString,
  labelI18N: Option[I18NString] = None,
  termI18N: Option[I18NString] = None
) {
  lazy val key: Symbol = Symbol(name)
  def name = nameI18N.c
  def name_en: String = nameI18N.en
  def name_ja: String = nameI18N.ja
  def name(p: Locale): String = nameI18N.as(p)
  def label: String = labelI18N.map(_.c) getOrElse term
  def label_en: String = labelI18N.map(_.en) getOrElse term_en
  def label_ja: String = labelI18N.map(_.ja) getOrElse term_ja
  def label(p: Locale): String = labelI18N.map(_.as(p)) getOrElse term(p)
  def term: String = termI18N.map(_.c) getOrElse name
  def term_en: String = termI18N.map(_.en) getOrElse name_en
  def term_ja: String = termI18N.map(_.ja) getOrElse name_ja
  def term(p: Locale): String = termI18N.map(_.as(p)) getOrElse name(p)
}

object Designation {
  val empty = Designation("")

  def apply(name: String): Designation = Designation(I18NString(name), None, None)

  def nameLabel(name: String, label: Option[I18NString]): Designation =
    Designation(I18NString(name), label, None)
  def nameLabel(name: String, label: I18NString): Designation =
    Designation(I18NString(name), Some(label), None)
  def nameLabel(name: String, label: String): Designation =
    Designation(I18NString(name), Some(I18NString(label)), None)

  trait Holder {
    def designation: Designation

    def name: String = designation.name
    def name_en: String = designation.name_en
    def name_ja: String = designation.name_ja
    def label: String = designation.label
    def label_en: String = designation.label_en
    def label_ja: String = designation.label_ja
    def term: String = designation.term
    def term_en: String = designation.term_en
    def term_ja: String = designation.term_ja
  }
}
