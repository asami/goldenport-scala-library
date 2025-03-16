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
 *  version Mar. 13, 2018
 *  version Dec. 27, 2018
 *  version Apr. 14, 2020
 *  version May. 18, 2020
 *  version Jun.  6, 2020
 *  version Mar. 15, 2021
 *  version Jun. 29, 2021
 *  version Mar.  6, 2022
 *  version Dec. 30, 2022
 *  version Jan.  4, 2023
 *  version Sep. 26, 2023
 * @version Mar.  9, 2025
 * @author  ASAMI, Tomoharu
 */
case class PathName(
  v: String,
  delimiter: String = PathName.DELIMITER
) {
  override def toString() = v

  def isEmpty: Boolean = v.isEmpty
  def head: String = firstComponent
  def headOption: Option[String] = components.headOption
  def tail: PathName = tailOption.getOrElse(throw new IllegalStateException("Empty PathName"))
  def tailOption: Option[PathName] = components.tail match {
    case Nil => None
    case xs => Some(PathName(xs.mkString(delimiter)))
  }
  lazy val components: List[String] = Strings.totokens(v, delimiter)
  lazy val firstComponent: String = components.headOption.getOrElse("")
  lazy val firstComponentBody: String = StringUtils.toPathnameBody(firstComponent)
  lazy val lastComponent: String = components.lastOption.getOrElse("")
  // deprecated
  lazy val lastConcreteComponent: String = {
    components.reverse.filterNot(_.contains("{")) match { // XXX "{"
      case Nil => ""
      case x :: _ => x
    }
  }
  def leaf: String = lastComponent
  lazy val leafBody: String = StringUtils.toPathnameBody(leaf)
  lazy val body: String = StringUtils.toPathnameBody(v)
  lazy val trunk: String = getTrunk.map(_.v) getOrElse ""
  lazy val suffix: String = getSuffix getOrElse ""
  lazy val getTrunk: Option[PathName] =
    if (isBase)
      None
    else
      Some(
        if (isAbsolute)
          PathName.createAbsolute(components.init)
        else
          PathName.createRelative(components.init)
      )
  lazy val getChild: Option[PathName] = components.tail match {
    case Nil => None
    case xs => Some(PathName(xs.mkString(delimiter)))
  }
  def getParent: Option[PathName] = components match {
    case Nil => None
    case xs => Some(PathName(xs.init.mkString(delimiter)))
  }
  def getSuffix: Option[String] = StringUtils.getSuffix(v) // lowercase
  def getSuffixLowerCase: Option[String] = StringUtils.getSuffixLowerCase(v)
  def getSuffixRaw: Option[String] = StringUtils.getSuffixRaw(v)
  def isSuffix(p: String): Boolean = getSuffix.fold(false)(_ == p)

  def get(i: Int): Option[String] = components.lift(i)

  def isBase: Boolean = v == "" || v == delimiter
  def isAbsolute: Boolean = v.startsWith(delimiter)

  def isAccept(p: List[String]): Boolean = {
    (p.length == components.length) && {
      p.zip(components).forall {
        case (l, r) => _is_pattern(r) || l == r
      }
    }
  }

  private def _is_pattern(s: String) = s.contains("{")

  // for HTTP
  def isResource(p: String): Boolean = firstComponent == p
  def isOperation(p: String): Boolean = firstComponent == p

  def +(p: PathName): PathName = :+(p)
  def :+(p: PathName): PathName = _concat_pathname(v, p.v)
  def +:(p: PathName): PathName = _concat_pathname(p.v, v)
  def +(p: String): PathName = :+(p)
  def :+(p: String): PathName = _concat_pathname(v, p)
  def +:(p: String): PathName = _concat_pathname(p, v)


  private def _concat_pathname(lhs: String, rhs: String) = copy(v = _concat_path(lhs, rhs))

  private def _concat_path(lhs: String, rhs: String) = delimiter match {
    case "/" => StringUtils.concatPath(lhs, rhs)
    case _ => s"${lhs}${delimiter}${rhs}"
  }

  def replaceFirst(p: String): PathName = {
    val a = components match {
      case Nil => Nil
      case x :: xs => p :: xs
    }
    if (isAbsolute)
      PathName(a.mkString(delimiter, delimiter, ""))
    else
      PathName(a.mkString(delimiter))
  }

  def length = components.length
}

object PathName {
  val home = PathName("")
  val DELIMITER = "/"

  def createAbsolute(delimiter: Char, ps: List[String]): PathName = PathName(delimiter + StringUtils.concatPath(delimiter, ps))
  def createAbsolute(ps: List[String]): PathName = createAbsolute('/', ps)
  def createAbsolute(p: String, pp: String, ps: String*): PathName = createAbsolute(p :: pp :: ps.toList)
  def createRelative(delimiter: Char, ps: List[String]): PathName = PathName(StringUtils.concatPath(delimiter, ps))
  def createRelative(ps: List[String]): PathName = createRelative('/', ps)
  def createRelative(p: String, pp: String, ps: String*): PathName = createRelative(p :: pp :: ps.toList)
//  def create(path: String): PathName = PathName(path)
//  def create(path: String, delimiter: String): PathName = PathName(path, delimiter)

  def getRelativePath(from: PathName, to: PathName): PathName =
    PathName(StringUtils.getRelativePath(from.v, to.v))
}
