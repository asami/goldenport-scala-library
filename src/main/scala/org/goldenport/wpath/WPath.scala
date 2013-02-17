package org.goldenport.wpath

import scala.util.parsing.combinator._
import scala.collection.immutable
import org.goldenport.util.ArrayMap

/**
 * @since   Jul.  2, 2010
 *  version Jan.  5, 2011
 * @version Feb. 17, 2013
 * @author  ASAMI, Tomoharu
 */
class WPath(val exprs: List[WExpr], val isRoot: Boolean, val isContainer: Boolean) {
  def createCursor() = new WPathCursor(exprs)
  def lastName = if (exprs == Nil) ""
             else exprs.last.toString
  def leafName = if (isContainer) ""
             else lastName
  def pathname = {
    (if (isRoot) "/" else "") +
    exprs.mkString("/") +
    (if (isContainer && !exprs.isEmpty) "/" else "")
  }
  def containerLeafName = if (isContainer) lastName
                  else if (exprs.length > 1) exprs(exprs.length - 2).toString
                  else ""
  def containerPathname = {
    (if (isRoot) "/" else "") +
    (if (exprs.length <= 1) "" else exprs.dropRight(1).mkString("/")) + "/"
  }
  def container = new WPath(exprs.dropRight(1), isRoot, true)

  def apply(idx: Int): WExpr = exprs.apply(idx)
  def length: Int = exprs.length
  def isEmpty = exprs.isEmpty

  // equals, hashCode, toString
}

case class WExpr(name: String, query: Option[WQuery], attribute: Option[String]) {
  override def toString(): String = {
    List(Some(name), query.map("?" + _), attribute.map("#" + _)).flatten.mkString
  }
}

case class WQuery(
  val queries: Map[String, String] = Map.empty,
  val feed_author: Option[String] = None,
  val feed_category: Option[WCategoryFilter] = None,
  val max_results: Option[Int] = None,
  val published_min: Option[String] = None,
  val published_max: Option[String] = None,
  val q: Option[String] = None,
  val start_index: Option[Int] = None,
  val strict_checking: Option[Boolean] = None,
  val updated_min: Option[String] = None,
  val updated_max: Option[String] = None)

object WQuery {
/*
  def apply(
    feed_author: String = null,
    feed_category: WCategoryFilter = null,
    max_results: Int = -1,
    published_min = null,
    published_max = null,
    q = null,
    start_index = null,
    strict_checking = null,
    updated_min = null,
    updated_max = null
  ): WQuery = {
    new WQuery(
      feed_author = lift(feed_author),
      feed_category = lift(feed_category),
      max_results = lift(max_results),
      published_max = lift(published_max),
      published_min = lift(published_min),
      q = lift(q),
      start_index = lift(start_index),
      strict_checking = lift(strict_checking),
      updated_min = lift(updated_min),
      updated_max = lift(updated_max)
    )
  }
*/

  def apply(query: Tuple2[String, String]*): WQuery = {
    apply(query.toList)
  }

  def apply(query: List[Tuple2[String, String]]): WQuery = {
    val qs = Map(query:_*)
    new WQuery(
      qs,
      qs.get("feed_author"),
      None,
      qs.get("max_results").map(_.toInt),
      qs.get("published_min"),
      qs.get("published_max"),
      qs.get("q"),
      qs.get("start_index").map(_.toInt),
      qs.get("strict_checking").map(_.toBoolean),
      None,
      None)
  }

  def apply(categories: WCategoryFilter): WQuery = {
    new WQuery(
      feed_category = Some(categories)
    )
  }
}

case class WCategoryFilter() {
  
}

class WPathCursor(val exprs: List[WExpr]) {
  private var next_index = 0

  def isEmpty = next_index >= exprs.length

  // mutable
  def next = {
    val index = next_index
    next_index += 1
    exprs(index)
  }

  def duplicate = {
    val cursor = new WPathCursor(exprs)
    cursor.next_index = next_index
    cursor
  }

  def remainders = exprs.drop(next_index)

  override def toString() = {
    "WPathCursor(" + exprs + "/" + next_index + "/" + isEmpty + ")"
  }
}

object WPath {
  def apply(expr: String): WPath = {
    WPathParser.path(expr).get
  }

  def lookups(path: String, provider: WPathProvider): List[AnyRef] = {
    val wpath = WPath(path)
    find_nodes(provider.root, wpath.createCursor).flatMap(_.content)
  }

  def lookupNodes(path: String, provider: WPathProvider): List[WPathNode] = {
    val wpath = WPath(path)
    find_nodes(provider.root, wpath.createCursor)
  }

  private def find_nodes(node: WPathNode, cursor: WPathCursor): List[WPathNode]  = {
    if (cursor.isEmpty) List(node)
    else {
      val children = node.find(cursor.next)
      children.flatMap(find_nodes(_, cursor.duplicate))
    }
  }
}

object WPathParser extends JavaTokenParsers {
  def path(string: String): ParseResult[WPath] = {
    parseAll(path, string)
  }

  def path: Parser[WPath] = (
    opt("/")~repsep(component, "/")~opt("/") ^^ {
      case root~exprs~container => new WPath(exprs, root.isDefined, container.isDefined)
    }
  )

  def component: Parser[WExpr] = node | category | queryNode

  def node: Parser[WExpr] = (
    label~opt(query)~opt(attribute) ^^ {
      case name~query~attribute => new WExpr(name, query, attribute)
    }
  )

  def category: Parser[WExpr] = (
    "-/"~categoryFilter ^^ {
      case _~categoryFilter => new WExpr("", Some(WQuery(categoryFilter)), None)
    }
  )

  def categoryFilter: Parser[WCategoryFilter] = (
    label ^^ {
      case label => new WCategoryFilter
    }
  )

  def query: Parser[WQuery] = (
    "?"~repsep(keyValues, "&") ^^ {
      case _~keyValues => WQuery(keyValues)
    }
  )

  def queryNode: Parser[WExpr] = (
    "?"~repsep(keyValues, "&") ^^ {
      case _~keyValues => WExpr("", Some(WQuery(keyValues)), None)
    }
  )

  def keyValues: Parser[Tuple2[String, String]] = {
    key~"="~value ^^ {
      case key~_~value => (key, value)
    }
  }

  def attribute: Parser[String] = {
    "#"~value ^^ {
      case _~attr => attr
    }
  }

  def label: Parser[String] = """[a-zA-Z0-9_.]+""".r
  def key: Parser[String] = """[a-zA-Z0-9_.]+""".r
  def value: Parser[String] = """[a-zA-Z0-9_.]+""".r
}
