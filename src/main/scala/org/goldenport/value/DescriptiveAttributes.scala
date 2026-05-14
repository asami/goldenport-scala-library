package org.goldenport.value

import java.util.Locale

import io.circe.Json
import org.goldenport.context.Consequence
import org.goldenport.config.ConfigLoader
import org.goldenport.i18n.I18NString
import org.goldenport.io.InputSource

/*
 * @since   May. 14, 2026
 * @version May. 14, 2026
 * @author  ASAMI, Tomoharu
 */
case class DescriptiveAttributes(
  headline: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  brief: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  summary: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  description: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  lead: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  `abstract`: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  remarks: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty,
  tooltip: DescriptiveAttributes.Text = DescriptiveAttributes.Text.empty
) {
  def isEmpty: Boolean =
    DescriptiveAttributes.Fields.forall(field(_).isEmpty)

  def orElse(rhs: DescriptiveAttributes): DescriptiveAttributes =
    DescriptiveAttributes(
      headline = headline.orElse(rhs.headline),
      brief = brief.orElse(rhs.brief),
      summary = summary.orElse(rhs.summary),
      description = description.orElse(rhs.description),
      lead = lead.orElse(rhs.lead),
      `abstract` = `abstract`.orElse(rhs.`abstract`),
      remarks = remarks.orElse(rhs.remarks),
      tooltip = tooltip.orElse(rhs.tooltip)
    )

  def field(name: String): DescriptiveAttributes.Text = name match {
    case "headline" => headline
    case "brief" => brief
    case "summary" => summary
    case "description" => description
    case "lead" => lead
    case "abstract" => `abstract`
    case "remarks" => remarks
    case "tooltip" => tooltip
    case _ => DescriptiveAttributes.Text.empty
  }

  def headlineString(locale: Locale): Option[String] = headline.get(locale)
  def briefString(locale: Locale): Option[String] = brief.get(locale)
  def summaryString(locale: Locale): Option[String] = summary.get(locale)
  def descriptionString(locale: Locale): Option[String] = description.get(locale)
  def leadString(locale: Locale): Option[String] = lead.get(locale)
  def abstractString(locale: Locale): Option[String] = `abstract`.get(locale)
  def remarksString(locale: Locale): Option[String] = remarks.get(locale)
  def tooltipString(locale: Locale): Option[String] = tooltip.get(locale)
}

object DescriptiveAttributes {
  val Fields: Vector[String] =
    Vector("headline", "brief", "summary", "description", "lead", "abstract", "remarks", "tooltip")

  val empty: DescriptiveAttributes = DescriptiveAttributes()

  case class Text(
    default: Option[String] = None,
    i18n: Map[String, String] = Map.empty
  ) {
    def isEmpty: Boolean = default.isEmpty && i18n.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def get(locale: Locale): Option[String] = {
      val tag = locale.toLanguageTag
      val lang = locale.getLanguage
      i18n.get(tag).orElse(i18n.get(lang)).orElse(default)
    }

    def getOrElse(locale: Locale, fallback: => String): String =
      get(locale).getOrElse(fallback)

    def orElse(rhs: Text): Text =
      Text(default.orElse(rhs.default), rhs.i18n ++ i18n)

    def toI18NString: Option[I18NString] =
      toI18NString(default.getOrElse(""))

    def toI18NString(fallback: => String): Option[I18NString] =
      if (isEmpty)
        None
      else {
        val defaultFallback = fallback
        val base = default.orElse(i18n.get("c")).getOrElse(defaultFallback)
        val en = i18n.get("en").orElse(default).getOrElse(defaultFallback)
        val ja = i18n.get("ja").orElse(default).getOrElse(defaultFallback)
        val locales = i18n.filterNot {
          case (k, _) => k == "en" || k == "ja" || k == "c"
        }.map {
          case (k, v) => Locale.forLanguageTag(k) -> v
        }
        Some(I18NString(base, en, ja, locales))
      }
  }

  object Text {
    val empty: Text = Text()
  }

  def load(in: InputSource): Consequence[DescriptiveAttributes] =
    ConfigLoader.loadConfig[Json](in).map(fromJson)

  def fromJson(json: Json): DescriptiveAttributes = {
    val nested = _nested(json)
    DescriptiveAttributes(
      headline = _text(json, nested, "headline"),
      brief = _text(json, nested, "brief"),
      summary = _text(json, nested, "summary"),
      description = _text(json, nested, "description"),
      lead = _text(json, nested, "lead"),
      `abstract` = _text(json, nested, "abstract"),
      remarks = _text(json, nested, "remarks"),
      tooltip = _text(json, nested, "tooltip")
    )
  }

  def fromJsonAt(json: Json, path: String*): DescriptiveAttributes = {
    val target = path.foldLeft(Option(json)) {
      case (Some(z), key) => z.hcursor.downField(key).focus
      case (None, _) => None
    }.getOrElse(Json.obj())
    fromJson(target)
  }

  def textFromJson(json: Json, name: String): Text = {
    val nested = _nested(json)
    _text(json, nested, name)
  }

  private def _nested(json: Json): Option[Json] =
    json.hcursor.downField("descriptive_attributes").focus.
      orElse(json.hcursor.downField("descriptiveAttributes").focus)

  private def _text(json: Json, nested: Option[Json], name: String): Text = {
    val nestedDefault = nested.flatMap(_string(_, name))
    val flatDefault = _string(json, name)
    val nestedI18n = nested.flatMap(_object_string_map(_, s"${name}_i18n")).getOrElse(Map.empty)
    val flatI18n = _object_string_map(json, s"${name}_i18n").getOrElse(Map.empty)
    Text(flatDefault.orElse(nestedDefault), nestedI18n ++ flatI18n)
  }

  private def _string(json: Json, name: String): Option[String] =
    json.hcursor.downField(name).as[String].toOption.map(_.trim).filter(_.nonEmpty)

  private def _object_string_map(json: Json, name: String): Option[Map[String, String]] =
    json.hcursor.downField(name).focus.flatMap { value =>
      value.asObject.map { obj =>
        obj.toMap.flatMap {
          case (k, v) => v.asString.map(s => k -> s)
        }.filter(_._2.trim.nonEmpty)
      }
    }.filter(_.nonEmpty)
}
