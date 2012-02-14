package org.goldenport.wpath

import scala.collection.mutable.ArrayBuffer

/**
 * @since   Jun. 22, 2010
 * @version Jul. 17, 2010
 * @author  ASAMI, Tomoharu
 **/
trait WPathProvider {
  type NODE <: WPathNode
  protected val root_Node: NODE

  def root: NODE = root_Node
}

trait WPathNode {
  type CONTENT
  type NODE <: WPathNode
  val name: String
  var content: Option[CONTENT] = None
  val children = new ArrayBuffer[NODE]
  var parent: Option[NODE] = None

  def find(expr: WExpr): List[NODE] = {
    children.filter(_.name == expr.name).filter(_.isMatch(expr.query)).toList
  }

  def isMatch(optquery: Option[WQuery]): Boolean = {
    optquery match {
      case Some(query) => is_Match(query)
      case None => true
    }
  }

  def pathName: String = {
    parent match {
      case Some(node) => "/" + path_name.mkString("/")
      case None => name
    }
  }

  private def path_name: List[String] = {
    parent match {
      case Some(node) => node.path_name ::: List(name)
      case None => Nil
    }
  }

  protected def is_Match(query: WQuery): Boolean = false

  def +=(name: String): NODE = {
    val node = create_Node(name)
    children += node
    node.parent = Some(this.asInstanceOf[node.NODE])
    node
  }

  protected def create_Node(name: String): NODE
}
