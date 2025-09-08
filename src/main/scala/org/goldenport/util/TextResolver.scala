package org.goldenport.util

import org.goldenport.Strings
import org.goldenport.context.Consequence
import org.goldenport.collection.NonEmptyVector
import org.goldenport.io.FileTextResolver
import org.goldenport.io.FileTextResolver.{Opt, Sub}
import org.goldenport.value._
import org.goldenport.values.NumberRange

/*
 * @since   Jul. 18, 2025
 *  version Jul. 19, 2025
 *  version Aug.  9, 2025
 * @version Sep.  7, 2025
 * @author  ASAMI, Tomoharu
 */
class TextResolver(context: TextResolver.Context) {
  import TextResolver._

  def resolve(p: String): Consequence[String] = Consequence {
    val s = Strings.tolines(p)
    val a = context.parameters.leveloffset.fold(s)(_level_offset(_, s))
    val b = context.parameters.lines.fold(a)(_lines(_, a))
    val c = _tags(context.parameters.effectiveTags, b)
    val d = context.parameters.indent.fold(c)(_indent(_, c))
    val e = context.parameters.options.fold(d)(x => _options(x.vector, d))
    val f = context.parameters.substitutes.fold(e)(x => _substitutes(x.vector, e))
    f.mkString(context.newline)
  }

  private def _level_offset(leveloffset: Int, p: Vector[String]): Vector[String] = {
    val markchar = context.sectionMarkChar
    val mark = markchar.toString
    p.map { line =>
      val trimmed = line.trim
      if (trimmed.startsWith(mark)) {
        val (prefix, rest) = trimmed.span(_ == markchar)
        val currentLevel = prefix.length
        val newLevel = (currentLevel + leveloffset).max(1)
        val newPrefix = mark * newLevel
        s"$newPrefix${rest.dropWhile(_ == ' ')}"
      } else {
        line
      }
    }
  }

  private def _lines(lines: NumberRange, ps: Vector[String]): Vector[String] = {
    val is = lines.indexes.toVector.distinct.sorted
    is.collect {
      case i if 0 <= i && i < ps.length => ps(i)
    }
  }

  private def _tags(tags: Option[NonEmptyVector[String]], p: Vector[String]): Vector[String] =
    tags match {
      case Some(s) => _tags(s.vector, p)
      case None => p
    }

  private def _tags(tags: Vector[String], p: Vector[String]): Vector[String] = {
    val TagStart = """//\s*tag::([^\[\]]+)(?:\[\])?""".r
    val TagEnd   = """//\s*end::([^\[\]]+)(?:\[\])?""".r
    // val TagStart = """tag::([^\[\]]+)(?:\[\])?""".r
    // val TagEnd   = """end::([^\[\]]+)(?:\[\])?""".r

    case class State(
      active: Set[String] = Set.empty,
      result: Vector[String] = Vector.empty,
      idx: Int = 0
    ) {
      def r = result

      def isActive: Boolean = active.exists(tags.contains)

      def +(rhs: String) = rhs.trim match {
        case TagStart(name) =>
          copy(active = active + name, idx = idx + 1)
        case TagEnd(name) =>
          copy(active = active - name, idx = idx + 1)
        case _ =>
          if (isActive)
            copy(result = result :+ rhs, idx = idx + 1)
          else
            copy(idx = idx + 1)
      }
    }

    p.foldLeft(State())(_+_).r
  }

  private def _indent(indent: String, p: Vector[String]): Vector[String] =
    if (indent.isEmpty)
      p
    else
      p.map(line => indent + line)

  private def _options(ops: Vector[Opt], p: Vector[String]): Vector[String] = {
    p // TODO
  }

  private def _substitutes(subs: Vector[Sub], p: Vector[String]): Vector[String] = {
    p // TODO
  }
}

object TextResolver {
  case class Context(
    parameters: FileTextResolver.Parameters = FileTextResolver.Parameters.empty
  ) {
    def withParameters(params: FileTextResolver.Parameters) =
      copy(parameters = params)

    def newline = "\n" // TODO
    def sectionMarkChar = '#' // TODO
  }

  object Context {
    val default = Context()
  }
}
