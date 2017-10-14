package org.goldenport.xml

import scalaz.{Node =>_, _} , Scalaz._
import scala.util.control.NonFatal
import scala.annotation.tailrec
import scala.xml._
import org.goldenport.Strings
import org.goldenport.util.AnyUtils

/*
 * @since   May. 25, 2014
 *  version Jun. 25, 2014
 *  version Aug. 30, 2017
 * @version Oct. 13, 2017
 * @author  ASAMI, Tomoharu
 */
object XmlUtils {
  def parseNode(p: String): Node = 
    if (p.startsWith("<"))
      XML.loadString(p)
    else
      Text(p)

  def parseNodeSeq(p: String): NodeSeq = 
    if (p.startsWith("<"))
      XML.loadString(p)
    else try {
      val s = s"<div>$p</div>"
      val x = XML.loadString(s)
      Group(x.child)
    } catch {
      case NonFatal(e) =>
        Text(p)
    }

  def makeString(p: String): String = try {
    parseNodeSeq(p).text
  } catch {
    case NonFatal(e) => p
  }

  def toString(nodes: Seq[Node]): String = nodes.map(_.text).mkString

  def asString(node: Node): String = {
    node match {
      case Text(s) => s
      case PCData(s) => s
      case elem: Elem => elem.child.toVector.foldMap(asString)
    }
  }

  def asXmlString(node: Node): String = {
    node match {
      case Text(s) => s
      case PCData(s) => s
      case elem: Elem => _normalize(elem).toString
    }
  }

  def contentsAsXmlString(node: Node): String = {
    node.child.toVector.foldMap(asXmlString)
  }

  private def _normalize(elem: Elem): Elem = {
    elem.copy(
      scope = _normalize_namespace(_prefixes(elem), elem.scope))
  }

  private def _prefixes(node: Node): Set[String] = {
    node match {
      case elem: Elem => Set(elem.prefix) ++ elem.child.map(_prefixes).toVector.concatenate
      case _ => Set.empty
    }
  }

  private def _normalize_namespace(prefixes: Set[String], ns: NamespaceBinding): NamespaceBinding = {
    def build(parent: NamespaceBinding, prefix: String): NamespaceBinding = {
      if (isAlreadyDefined(parent, prefix)) parent
      else findNamespace(ns, prefix) match {
        case Some(s) => s.copy(parent = parent)
        case None => parent
      }
    }
    prefixes.foldLeft(TopScope: NamespaceBinding)(build)
  }

  @tailrec
  def isAlreadyDefined(ns: NamespaceBinding, prefix: String): Boolean = {
    ns match {
      case TopScope => false
      case _ if ns.prefix == prefix => true
      case _ => isAlreadyDefined(ns.parent, prefix)
    }
  }

  @tailrec
  def findNamespace(ns: NamespaceBinding, prefix: String): Option[NamespaceBinding] = {
    ns match {
      case TopScope => None
      case _ if ns.prefix == prefix => Some(ns)
      case _ => findNamespace(ns.parent, prefix)
    }
  }

  def findFirstElementByLabel(label: String, s: String): Option[Elem] = {
    (XML.loadString(s) \\ label).headOption.asInstanceOf[Option[Elem]]
  }

  @annotation.tailrec
  def isAttribute(attrs: MetaData, key: String): Boolean =
    if (attrs == null)
      false
    else
      attrs.key == key || isAttribute(attrs.next, key)

  @annotation.tailrec
  def isNonEmptyAttribute(attrs: MetaData, key: String): Boolean =
    if (attrs == Null)
      false
    else
      (attrs.key == key && isEmptyValue(attrs.value)) || isNonEmptyAttribute(attrs.next, key)

  def isEmptyValue(p: Seq[Node]): Boolean = p.toList match {
    case Nil => true
    case x :: Nil => x.text == ""
    case xs => false
  }

  def getAttribute(elem: Elem, key: String, default: String): String = getAttribute(elem.attributes, key).getOrElse(default)

  def getAttribute(elem: Elem, key: String): Option[String] = getAttribute(elem.attributes, key)

  def getAttribute(attrs: MetaData, key: String): Option[String] =
    Option(attrs(key)).map(_.map(_.text).mkString)

  def setAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    setOrRemoveAttribute(elem, key, value)

  def updateAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    setAttributeIfRequired(elem, key, value)

  def complementAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    complementAttributeIfRequired(elem, key, value)

  def setOrRemoveAttribute(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(removeAttribute(elem, key))(setAttribute(elem, key, _))

  def setAttributeIfRequired(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(elem)(setAttribute(elem, key, _))

  def complementAttributeIfRequired(elem: Elem, key: String, value: Option[String]): Elem =
    value.fold(elem)(complementAttribute(elem, key, _))

  def setAttribute(elem: Elem, key: String, value: String): Elem =
    elem.copy(attributes = setAttribute(elem.attributes, key, value))

  def setAttribute(attrs: MetaData, key: String, value: String): MetaData =
    if (attrs == Null)
      attrs
    else if (isAttribute(attrs, key)) {
      if (attrs.key == key)
        new UnprefixedAttribute(key, value, setAttribute(attrs.next, key, value))
      else
        attrs.copy(setAttribute(attrs.next, key, value))
    } else {
      new UnprefixedAttribute(key, value, attrs)
    }

  def complementAttribute(elem: Elem, key: String, value: String): Elem = 
    elem.copy(attributes = complementAttribute(elem.attributes, key, value))

  def complementAttribute(attrs: MetaData, key: String, value: String): MetaData =
    if (isAttribute(attrs, key))
      attrs
    else
      new UnprefixedAttribute(key, value, attrs)

  def removeAttribute(elem: Elem, key: String): Elem = 
    elem.copy(attributes = removeAttribute(elem.attributes, key))

  def removeAttribute(attrs: MetaData, key: String): MetaData =
    attrs.remove(key)

  def nodeSeqToNodeList(ps: NodeSeq): List[Node] = ps match {
    case Group(ms) => ms.toList
    case m: Node => List(m)
    case m => m.toList
  }

  def seqOfNodeSeqToSeqOfNode(p: Seq[NodeSeq]): Seq[Node] = p.flatMap {
    case Group(ms) => seqOfNodeSeqToSeqOfNode(ms)
    case m: Node => List(m)
    case m => seqOfNodeSeqToSeqOfNode(m.toList)
  }

  def concat(lhs: NodeSeq, rhs: NodeSeq): NodeSeq = {
    val a = nodeSeqToNodeList(lhs) ::: nodeSeqToNodeList(rhs)
    a match {
      case Nil => Group(Nil)
      case x :: Nil => x
      case xs => Group(xs)
    }
  }

  def nodesToNode(ps: Seq[Node]): Node = nodesToNodeOption(ps).getOrElse(Group(Nil))

  def nodesToNodeOption(ps: Seq[Node]): Option[Node] = {
    def flatten(xs: List[Node]): List[Node] = xs.flatMap {
      case Group(gs) => gs.flatMap(nodesToNodeOption)
      case m => Some(m)
    }
    val a = flatten(ps.toList)
    a match {
      case Nil => None
      case x :: Nil => Some(x)
      case xs => Some(Group(xs))
    }
  }

  def attributes(attrs: Seq[(String, Any)]): MetaData = attrs.toList match {
    case Nil => Null
    case x :: xs => attributes(x, attributes(xs))
  }

  def attributes(p: (String, Any), next: MetaData): MetaData = {
    val (name, value) = p
    val v = AnyUtils.toString(value) // TODO
    val i = name.indexOf(':')
    if (i == -1) {
      new UnprefixedAttribute(name, v, next)
    } else {
      val prefix = name.substring(0, i)
      val label = name.substring(i + 1)
      new PrefixedAttribute(prefix, label, v, next)
    }
  }

  def element(name: String, attrs: Seq[(String, Any)], children: Seq[Node]): Elem =
    Elem(null, name, attributes(attrs), TopScope, false, children: _*)

  def toSummary(s: String): String = dom.DomUtils.toSummary(s)
}