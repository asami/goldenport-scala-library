package org.goldenport.context

import org.goldenport.value._
import org.goldenport.i18n.StringFormatter
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils

/*
 * @since   Dec. 20, 2021
 *  version Mar. 19, 2022
 *  version Aug. 21, 2023
 *  version Mar. 15, 2025
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait Showable extends org.goldenport.extension.Showable {
  def display = show
  def show = print
}

object Showable {
  trait Base extends Showable {
    override def toString() = show

//    protected def show_Name: String
    protected def print_String: String
    protected def display_String: String
    protected def show_String: String

    protected def label_string = StringUtils.capitalize(getClass.getName)

    protected final def escape_string(p: String) =
      StringFormatter.display.escapeDisplay(p)

    protected final def to_print(p: Any): String = AnyUtils.toPrint(p)
    protected final def to_display(p: Any): String = AnyUtils.toDisplay(p)
    protected final def to_show(p: Any): String = AnyUtils.toShow(p)
  }

  trait Control extends Base {
    override def print = s"[${label_string}]${print_String}"

    override def display = s"[${label_string}]${escape_string(display_String)}"
    override def show = s"[${label_string}]${escape_string(show_String)}"

    override protected def print_String: String = show_String

    override protected def display_String: String = show_String
  }
  trait Value extends Base {
    protected def show_Name: String

    protected override def label_string = StringUtils.capitalize(show_Name)

    override def print = print_String

    override def display = s"[${label_string}]${escape_string(display_String)}"
    override def show = s"[${label_string}]${escape_string(show_String)}"

    override protected def display_String: String = show_String

    override protected def show_String: String = print_String
  }

  sealed trait Kind extends NamedValueInstance {
  }
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(Print, Display, Show, Literal, Embed)

    case object Print extends Kind {
      val name = "print"
    }
    case object Display extends Kind {
      val name = "display"
    }
    case object Show extends Kind {
      val name = "show"
    }
    case object Literal extends Kind {
      val name = "literal"
    }
    case object Embed extends Kind {
      val name = "embed"
    }
  }

  def toString(kind: Kind, o: Any): String = kind match {
    case Kind.Print => toPrint(o)
    case Kind.Display => toDisplay(o)
    case Kind.Show => toShow(o)
    case Kind.Literal => toLiteral(o)
    case Kind.Embed => toEmbed(o)
  }

  def toPrint(o: Any): String = AnyUtils.toPrint(o)
  def toDisplay(o: Any): String = AnyUtils.toDisplay(o)
  def toShow(o: Any): String = AnyUtils.toShow(o)
  def toLiteral(o: Any): String = AnyUtils.toLiteral(o)
  def toEmbed(o: Any): String = AnyUtils.toEmbed(o)
}
