package org.goldenport.xml

import scalaz.{Node =>_, _} , Scalaz._
import scala.util.control.NonFatal
import scala.annotation.tailrec
import scala.xml._
import com.asamioffice.goldenport.xml.UXML
import org.goldenport.Strings
import org.goldenport.util.{AnyUtils, SeqUtils}

/*
 * @since   May. 25, 2014
 *  version Jun. 25, 2014
 *  version Aug. 30, 2017
 *  version Oct. 17, 2017
 *  version Nov. 15, 2017
 *  version Jan. 12, 2018
 *  version Feb. 18, 2018
 *  version Aug.  5, 2018
 *  version Mar. 28, 2022
 * @version Dec. 29, 2023
 * @author  ASAMI, Tomoharu
 */
object XmlUtils {
  val emptyNodeSeq: NodeSeq = Group(Nil)

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

  def tagName(p: Elem): String =
    Option(p.prefix).fold(p.label)(x => s"$x:${p.label}")

  @annotation.tailrec
  def isAttribute(attrs: MetaData, key: String): Boolean =
    if (attrs == Null)
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

  def attributeList(elem: Elem): List[(String, String)] = attributeVector(elem).toList
  def attributeList(p: MetaData): List[(String, String)] = attributeVector(p).toList

  def attributeVector(elem: Elem): Vector[(String, String)] = attributeVector(elem.attributes)
  def attributeVector(p: MetaData): Vector[(String, String)] = {
    @annotation.tailrec
    def go(x: MetaData, r: Vector[(String, String)]): Vector[(String, String)] = {
      if (x == Null || x == null)
        r
      else
        go(x.next, r :+ (x.key -> text(x)))
    }
    go(p, Vector.empty)
  }

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
    val a: List[Node] = nodeSeqToNodeList(lhs) ::: nodeSeqToNodeList(rhs)
    nodesToNodeSeq(a)
  }

  def concat(ps: Seq[NodeSeq]): NodeSeq = ps.toList match {
    case Nil => Group(Nil)
    case xs => 
      val a = xs./:(List[Node]())((z, x) => z ::: nodeSeqToNodeList(x))
      nodesToNodeSeq(a)
  }

  def nodesToNodeSeq(ps: Seq[Node]): NodeSeq = ps.toList match {
    case Nil => Group(Nil)
    case x :: Nil => x
    case xs => Group(xs)
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

  def attributesOption(a: (String, Option[String]), as: Seq[(String, Option[String])], next: MetaData): MetaData =
    attributes(SeqUtils.buildTupleVector(a +: as), next)

  def attributesOption(ps: Seq[(String, Option[String])], next: MetaData): MetaData =
    attributes(SeqUtils.buildTupleList(ps), next)

  def attributesOption(ps: Seq[(String, Option[String])]): MetaData =
    attributes(SeqUtils.buildTupleList(ps))

  def attributesOption(fixes: Seq[(String, String)], options: Seq[(String, Option[String])]): MetaData =
    attributes(fixes, attributesOption(options))

  def attributes(a: (String, String), as: Seq[(String, String)], next: MetaData): MetaData =
    attributes(a +: as, next)

  def attributes(attrs: Seq[(String, String)], next: MetaData): MetaData = attrs.toList match {
    case Nil => next
    case x :: xs => attributes(x, attributes(xs, next))
  }

  def attributes(attrs: Seq[(String, String)]): MetaData = attrs.toList match {
    case Nil => Null
    case x :: xs => attributes(x, attributes(xs))
  }

  def attributes(p: (String, String), next: MetaData): MetaData = {
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

  def text(p: MetaData): String = text(p.value)
  def text(ps: Seq[Node]): String = ps.map(_.text).mkString

  def element(name: String, attrs: Seq[(String, String)]): Elem =
    element(name, attrs, Nil)

  def element(name: String, attrs: Seq[(String, String)], children: Seq[Node]): Elem =
    Elem(null, name, attributes(attrs), TopScope, false, children: _*)

  def elementWithAttributesOption(
    name: String,
    attrs: Seq[(String, Option[String])]
  ): Elem =
    elementWithAttributesOption(name, attrs, Nil)

  def elementWithAttributesOption(
    name: String,
    attrs: Seq[(String, Option[String])],
    children: Seq[Node]
  ): Elem =
    Elem(null, name, attributesOption(attrs), TopScope, false, children: _*)

  def elementWithAttributesFixOption(
    name: String,
    fixes: Seq[(String, String)],
    options: Seq[(String, Option[String])]
  ): Elem =
    elementWithAttributesFixOption(name, fixes, options, Nil)

  def elementWithAttributesFixOption(
    name: String,
    fixes: Seq[(String, String)],
    options: Seq[(String, Option[String])],
    children: Seq[Node]
  ): Elem =
    Elem(null, name, attributesOption(fixes, options), TopScope, false, children: _*)

  def appendAttributes(elem: Elem, a: (String, Option[String]), as: (String, Option[String])*): Elem =
    elem.copy(attributes = attributesOption(a, as, elem.attributes))

  def toSummary(s: String): String = dom.DomUtils.toSummary(s)

  def adjustEmptyDiv(p: Elem): Elem = 
    if (isEmptyAttributeDiv(p)) {
      p.child.toList match {
        case Nil => p
        case x :: Nil => x match {
          case m: Elem => adjustEmptyDiv(m)
          case _ => p
        }
        case _ =>
          val a = p.child.toList.flatMap(_adjust_empty_div_node)
          p.copy(child = a)
      }
    } else {
      p.copy(child = p.child.toList.flatMap(_adjust_empty_div_node))
    }

  def adjustEmptyDivNode(p: Node): Node = p match {
    case Group(ms) => Group(ms.map(adjustEmptyDivNode))
    case m: Elem => adjustEmptyDiv(m)
    case m => m
  }

  private def _adjust_empty_div(p: Elem): Option[Node] =
    if (isEmptyAttributeDiv(p)) {
      p.child.toList match {
        case Nil => None
        case x :: Nil => x match {
          case m: Elem => _adjust_empty_div(m)
          case _ => Some(p)
        }
        case _ => p.child.flatMap(_adjust_empty_div_node).toList match {
          case Nil => None
          case xs => Some(Group(xs))
        }
      }
    } else {
      Some(p.copy(child = p.child.toList.flatMap(_adjust_empty_div_node)))
    }

  private def _adjust_empty_div_node(p: Node): Option[Node] = p match {
    case Group(ms) => ms.flatMap(_adjust_empty_div_node).toList match {
      case Nil => None
      case xs => Some(Group(xs))
    }
    case m: Elem => _adjust_empty_div(m)
    case m => Some(m)
  }

  def orEmptyNodeSeq(p: Option[NodeSeq]): NodeSeq = p getOrElse emptyNodeSeq

  def isEmptyAttributeDiv(p: Elem): Boolean = {
    val a = p.label == "div"
    def b = p.attributes.filterNot(x =>
      (x.key == "class" || x.key == "style") && XmlUtils.isEmptyValue(x.value)).isEmpty
    a && b
  }

  def escape(s: String) = UXML.escape(s)
  def escapeEntityQuot(s: String) = UXML.escapeEntityQuot(s)
  def escapeEntityApos(s: String) = UXML.escapeEntityApos(s)
  def escapeAttrQuot(s: String) = UXML.escapeAttrQuot(s)
  def escapeAttrApos(s: String) = UXML.escapeAttrApos(s)
  def escapeSystemQuot(s: String) = UXML.escapeSystemQuot(s)
  def escapeSystemApos(s: String) = UXML.escapeSystemApos(s)
  def escapeCharData(s: String) = UXML.escapeCharData(s)
  def escapeCharDataCr(s: String) = UXML.escapeCharDataCr(s)

  def show(p: Elem): String = {
    val attrs = attributeVector(p).map {
      case (k, v) => s"${k}=${v}"
    }.mkString(",")
    s"""${tagName(p)}(${attrs})"""
  }
}
