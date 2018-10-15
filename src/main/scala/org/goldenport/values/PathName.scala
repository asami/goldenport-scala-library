package org.goldenport.values

import org.goldenport.Strings
import org.goldenport.util.StringUtils

/*
 * See org.goldenport.wpath.WPath
 * See com.everforth.everforth.util.PathAccumulator
 * See com.everforth.lib.util.StringUtils
 *
 * @since   Jan. 10, 2017
 *  version Apr. 16, 2017
 *  version May. 18, 2017
 *  version Jul. 25, 2017
 *  version Aug. 29, 2017
 *  version Nov.  6, 2017
 *  version Jan. 12, 2018
 * @version Mar. 13, 2018
 * @author  ASAMI, Tomoharu
 */
case class PathName(v: String) {
  override def toString() = v

  lazy val components: List[String] = Strings.totokens(v, "/")
  lazy val firstComponent: String = components.headOption.getOrElse("")
  lazy val lastConcreteComponent: String = {
    components.reverse.filterNot(_.contains("{")) match { // XXX "{"
      case Nil => ""
      case x :: _ => x
    }
  }
  lazy val getParent: Option[PathName] =
    if (isBase)
      None
    else
      Some(PathName(components.init))
  def isBase: Boolean = v == "" || v == "/"
  def isAbsolute: Boolean = v.startsWith("/")

  def isAccept(p: List[String]): Boolean = {
    (p.length == components.length) && {
      p.zip(components).forall {
        case (l, r) => _is_pattern(r) || l == r
      }
    }
  }

  private def _is_pattern(s: String) = s.contains("{")

  def isResource(p: String): Boolean = firstComponent == p
  def isOperation(p: String): Boolean = firstComponent == p

  def +(p: String): PathName = PathName(StringUtils.concatPath(v, p))
  def +:(p: String): PathName = PathName(StringUtils.concatPath(p, v))

  def replaceFirst(p: String): PathName = {
    val a = components match {
      case Nil => Nil
      case x :: xs => p :: xs
    }
    if (isAbsolute)
      PathName(a.mkString("/", "/", ""))
    else
      PathName(a.mkString("/"))
  }

  def length = components.length
  def body = StringUtils.toPathnameBody(v)
  def getSuffix: Option[String] = StringUtils.getSuffix(v)
}

object PathName {
  val home = PathName("")

  def apply(ps: List[String]): PathName = PathName(StringUtils.concatPath(ps))
  def apply(p: String, pp: String, ps: String*): PathName = apply(p :: pp :: ps.toList)
}
