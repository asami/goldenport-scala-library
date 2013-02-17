package org.goldenport.wpath

import scala.collection.mutable.ArrayBuffer

/**
 * @since   Jun. 22, 2010
 *  version Jul. 17, 2010
 * @version Feb. 17, 2013
 * @author  ASAMI, Tomoharu
 */
trait WPathProvider {
  type NODE <: WPathNode
  protected val root_Node: NODE

  def root: NODE = root_Node
}

trait WPathNode {
  type CONTENT <: AnyRef
  type NODE <: WPathNode
  val name: String
  var content: Option[CONTENT] = None
  private val _children = new ArrayBuffer[NODE]
  private var _parent: Option[NODE] = None
  private var _dirty: Boolean = true

  protected def ensure_node {
    if (_dirty) {
      _children ++= load_Children
      _dirty = false
    }
  }

  protected def load_Children: Seq[NODE] = Nil

  def find(expr: WExpr): List[NODE] = {
    ensure_node
    _children.filter(_.name == expr.name).filter(_.isMatch(expr.query)).toList
  }

  def isMatch(optquery: Option[WQuery]): Boolean = {
    ensure_node
    optquery match {
      case Some(query) => is_Match(query)
      case None => true
    }
  }

  def pathName: String = {
    ensure_node
    _parent match {
      case Some(node) => "/" + path_name.mkString("/")
      case None => name
    }
  }

  private def path_name: List[String] = {
    _parent match {
      case Some(node) => node.path_name ::: List(name)
      case None => Nil
    }
  }

  protected def is_Match(query: WQuery): Boolean = false

  def +=(name: String): NODE = {
    val node = create_Node(name)
    _children += node
    node._parent = Some(this.asInstanceOf[node.NODE])
    node
  }

  protected def create_Node(name: String): NODE

  def childNodes: Seq[NODE] = {
    ensure_node
    _children
  }

  def childContents: Seq[NODE#CONTENT] = {
    ensure_node
    _children.flatMap(_.content)
  }

  override def toString() = {
    getClass.getSimpleName + "(" + name + "/" + _children.length + ")"
  }
}
