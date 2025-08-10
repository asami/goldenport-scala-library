package org.goldenport.io

import scala.util.Try
import java.io.File
import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.collection.NonEmptyVector
import org.goldenport.value._
import org.goldenport.values.NumberRange
import org.goldenport.util.TextResolver
import org.goldenport.util.NumberUtils
import org.goldenport.util.StringUtils

/*
 * @since   Jul. 18, 2025
 * @version Aug. 10, 2025
 * @author  ASAMI, Tomoharu
 */
class FileTextResolver(context: FileTextResolver.Context) {
  def resolve(path: String): Consequence[String] = {
    val fr = new FileResolver(context.fileResolverContext)
    val tr = new TextResolver(context.textResolverContext)
    for {
      x <- fr.resolveString(path)
      s <- tr.resolve(x)
    } yield s
  }
}

object FileTextResolver {
  case class Context(
    fileResolverContext: FileResolver.Context = FileResolver.Context.default,
    textResolverContext: TextResolver.Context = TextResolver.Context.default
  ) {
    def withParameters(params: Parameters) = {
      copy(
        fileResolverContext = fileResolverContext.withParameters(params),
        textResolverContext = textResolverContext.withParameters(params)
      )
    }

    def withBaseFile(p: URI) = {
      copy(fileResolverContext = fileResolverContext.withBaseFile(p))
    }
  }
  object Context {
    val default = Context()
  }

  case class Parameters(
    leveloffset: Option[Int] = None,
    lines: Option[NumberRange] = None,
    tag: Option[String] = None,
    tags: Option[NonEmptyVector[String]] = None,
    indent: Option[String] = None,
    encoding: Option[String] = None,
    options: Option[NonEmptyVector[Opt]] = None,
    substitutes: Option[NonEmptyVector[Sub]] = None
  ) {
    def effectiveTags: Vector[String] = tags match {
      case Some(s) => s.vector
      case None => Vector.empty
    }
  }
  object Parameters {
    val empty = Parameters()

    def parse(attrs: Map[String, String]): Consequence[Parameters] = {
      for {
        leveloffset <- _parse_int(attrs.get("lineoffset"))
        lines <- _parse_range(attrs.get("lines"))
        tag <- _parse_string(attrs.get("tag"))
        tags <- _parse_string_list(attrs.get("tags"))
        indent <- _parse_string(attrs.get("indent"))
        encoding <- _parse_string(attrs.get("encoding"))
        options <- _parse_options(attrs.get("opts"))
        substitutes <- _parse_substitutes(attrs.get("subs"))
      } yield Parameters(
        leveloffset,
        lines,
        tag,
        tags,
        indent,
        encoding,
        options,
        substitutes
      )
    }

    private def _parse_int(p: Option[String]): Consequence[Option[Int]] =
      p match {
        case Some(s) => NumberUtils.consequenceInt(s).map(Some(_))
        case None => Consequence.none[Option[Int]]
      }

    private def _parse_string(p: Option[String]): Consequence[Option[String]] =
      p match {
        case Some(s) => Consequence.success(Some(s))
        case None => Consequence.none[Option[String]]
      }

    private def _parse_string_list(p: Option[String]): Consequence[Option[NonEmptyVector[String]]] =
      p match {
        case Some(s) => Consequence(StringUtils.makeOptionNonEmptyVectorToken(s))
        case None => Consequence.none[Option[NonEmptyVector[String]]]
      }

    private def _parse_range(p: Option[String]): Consequence[Option[NumberRange]] =
      Consequence.runOptionMap(p)(NumberRange.parseC)

    private def _parse_options(p: Option[String]): Consequence[Option[NonEmptyVector[Opt]]] =
      Consequence.runOptionMap(p)(Opt.parseNonEmptyVector(_, "+"))

    private def _parse_substitutes(p: Option[String]): Consequence[Option[NonEmptyVector[Sub]]] =
      Consequence.runOptionMap(p)(Sub.parseNonEmptyVector(_, "+"))
  }

  sealed trait Opt extends NamedValueInstance
  object Opt extends EnumerationClass[Opt] {
    val elements = Vector(
      Optional,
      Inline,
      Default,
      Nowrap,
      Noheader,
      Header,
      Unbreakable,
      Autowidth,
      Breakable
    )

    case object Optional extends Opt {
      val name = "optional"
    }
    case object Inline extends Opt {
      val name = "inline"
    }
    case object Default extends Opt {
      val name = "default"
    }
    case object Nowrap extends Opt {
      val name = "nowrap"
    }
    case object Noheader extends Opt {
      val name = "noheader"
    }
    case object Header extends Opt {
      val name = "header"
    }
    case object Unbreakable extends Opt {
      val name = "unbreakable"
    }
    case object Autowidth extends Opt {
      val name = "autowidth"
    }
    case object Breakable extends Opt {
      val name = "breakable"
    }
  }

  sealed trait Sub extends NamedValueInstance
  object Sub extends EnumerationClass[Sub] {
    val elements = Vector(
      Attributes,
      Macros,
      Quotes,
      Replacements,
      Specialcharacters,
      Callouts,
      Normal,
      Verbatim,
      NoneSub
    )

    case object Attributes extends Sub {
      val name = "attributes"
    }
    case object Macros extends Sub {
      val name = "macos"
    }
    case object Quotes extends Sub {
      val name = "quotes"
    }
    case object Replacements extends Sub {
      val name = "replacements"
    }
    case object Specialcharacters extends Sub {
      val name = "specialcharacters"
    }
    case object Callouts extends Sub {
      val name = "callouts"
    }
    case object Normal extends Sub {
      val name = "normal"
    }
    case object Verbatim extends Sub {
      val name = "verbatim"
    }
    case object NoneSub extends Sub {
      val name = "none"
    }
  }

  def create(): FileTextResolver = new FileTextResolver(Context.default)
}
