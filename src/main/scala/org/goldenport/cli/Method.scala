package org.goldenport.cli

import org.goldenport.util.AnyRefUtils
import Environment.AppEnvironment
import org.goldenport.Strings

/*
 * @since   Feb. 24, 2019
 *  version Mar.  6, 2019
 * @version Oct. 15, 2019
 * @author  ASAMI, Tomoharu
 */
trait Method {
  def call: OperationCall
  def run: Response = execute
  def execute: Response

  def environment = call.environment
  def toAppEnvironment[T <: AppEnvironment]: T = environment.toAppEnvironment

  protected final def newline = call.environment.newline

  protected final def head_string_option: Option[String] =
    call.request.arguments.headOption.map(x => AnyRefUtils.toString(x.value))

  protected final def to_response(p: String) = StringResponse(p)

  protected final def to_response_lines_string(p: String) = to_response(build_lines_string(p))

  protected final def build_lines(s: String): String = build_lines(Strings.tolines(s))

  protected final def build_lines(ps: Seq[String]): String = ps.mkString("", newline, newline)

  protected final def build_lines_string(s: String): String = build_lines_string(Strings.tolines(s))

  protected final def build_lines_string(ps: Seq[String]): String = ps.mkString(newline)

  protected final def build_lines_string_with_number(ps: Seq[String]): String = {
    val xs = ps.zipWithIndex
    val numwidth = _number_width(ps.length)
    xs.map(_with_number(numwidth, _)).mkString(newline)
  }

  private def _number_width(int: Long): Int = 2 // TODO

  private def _with_number(w: Int, p: (String, Int)): String = {
    val (s,n) = p
    val num = (n + 1).toString
    val length = num.length
    val numpart =
      if (length >= w)
        num
      else
        (Vector.fill(w - length)(' ') ++ num.toVector).mkString
    s"$numpart: $s"
  }
}
