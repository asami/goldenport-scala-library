package org.goldenport.cli

import java.net.{URL, URI}
import org.goldenport.Strings
import org.goldenport.context.{Consequence, Conclusion}
import org.goldenport.bag.StringBag
import org.goldenport.bag.ChunkBag
import org.goldenport.util.AnyRefUtils
import Environment.AppEnvironment

/*
 * @since   Feb. 24, 2019
 *  version Mar.  6, 2019
 *  version Oct. 15, 2019
 *  version Jun. 18, 2021
 *  version Jan. 30, 2022
 * @version Feb.  1, 2022
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

  protected final def to_response_file(uri: URI, p: String) = {
    val bag = StringBag.create(p)
    FileResponse(bag, uri)
  }

  protected final def to_response_file(url: URL, p: String) = {
    val bag = StringBag.create(p)
    FileResponse(bag, url)
  }

  protected final def to_response_file(url: URL, p: ChunkBag) = FileResponse(p, url)

  protected final def to_response_file(p: ChunkBag) = FileResponse(p)

  protected final def to_response(p: Conclusion): Response = ConclusionResponse(p)

  // protected final def to_response(p: Consequence[String]): Response = p match {
  //   case Consequence.Success(r, _) => to_response(r)
  //   case Consequence.Error(c) => to_response(c)
  // }

  protected final def to_response(p: Consequence[Response]): Response = p match {
    case Consequence.Success(r, _) => r
    case Consequence.Error(c) => to_response(c)
  }

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
