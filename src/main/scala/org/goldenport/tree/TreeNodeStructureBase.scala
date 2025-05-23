package org.goldenport.tree

import scala.xml._
import scala.collection.mutable.ArrayBuffer
import java.util.Locale
import java.util.UUID
import com.asamioffice.goldenport.xml.XmlAttributeBuffer
import org.goldenport.Strings
import org.goldenport.values.PathName
import org.goldenport.values.CompactUuid

/**
 * @since   Aug. 13, 2008
 *  version Sep. 19, 2011
 *  version Feb. 22, 2012
 *  version Nov. 18, 2019
 *  version Nov. 14, 2020
 *  version Dec. 26, 2020
 *  version Feb. 23, 2025
 *  version Mar.  7, 2025
 * @version Apr. 24, 2025
 * @author  ASAMI, Tomoharu
 */
trait TreeNodeStructureBase[E] extends TreeNode[E] {
  val id: String = UUID.randomUUID.toString
  private var node_title: String = null
  private var node_parent: TreeNode[E] = null
  private val node_children = new ArrayBuffer[TreeNode[E]]()
  private var node_facade: Tree[E] = null
  private var is_filled_children = false
  private var is_modified = false
  private var xml_attributes = new XmlAttributeBuffer

  override def title: String = node_title
  override def title_=(s: String) = node_title = s

  override def isRoot: Boolean = { parent == null }

  override def isContainer: Boolean = {
    is_Container match {
      case Some(true) => true
      case Some(false) => false
      case None => !isEmpty || content == null
    }
  }

  protected def is_Container: Option[Boolean] = None

  override def isLeaf: Boolean = {
    is_Leaf match {
      case Some(true) => true
      case Some(false) => false
      case None => isEmpty && content != null
    }
  }

  protected def is_Leaf: Option[Boolean] = None

  override def isEmpty: Boolean = node_children.isEmpty
  override def isVoid: Boolean = isEmpty && content == null
  override def parent: TreeNode_TYPE = node_parent.asInstanceOf[TreeNode_TYPE]
  override def parent_=(aParent: TreeNode[E]) { node_parent = aParent }
  override def facade: Tree[E] = node_facade
  override def facade_=(aFacade: Tree[E]) { node_facade = aFacade }

  override def length: Int = {
    fill_children()
    node_children.length
  }

  override def getChild(index: Int): TreeNode_TYPE = {
    fill_children()
    node_children(index).asInstanceOf[TreeNode_TYPE]
  }

  override def getChild(name: String): Option[TreeNode_TYPE] = {
    fill_children()
    node_children.find(_.name == name).asInstanceOf[Option[TreeNode_TYPE]]
  }

  override def setChild(name: String): TreeNode_TYPE = {
    fill_children()
    val mayNode = getChild(name)
    if (mayNode.isDefined) mayNode.get
    else set_child(new_Node(name))
  }

  override def setChild(name: String, content: E): TreeNode_TYPE = {
    fill_children()
    val mayNode = getChild(name)
    val node = mayNode getOrElse set_child(new_Node(name))
    node.content = content
    node
  }

  override def addChild(): TreeNode_TYPE = {
    addChild(_make_anon_node())
  }

  private def _make_anon_node(): TreeNode_TYPE = new_Node(_make_node_name)

  private def _make_node_name() = CompactUuid.generateString()

  override def addChild(child: TreeNode[E]): TreeNode_TYPE = {
    set_child(child)
    set_Child(child)
    child.asInstanceOf[TreeNode_TYPE]
  }

  override def addChildren(ps: Seq[TreeNode[E]]) {
    ps.foreach(addChild)
  }

  override def addChildren(parent: TreeNode[E]) {
    for (child <- parent.children) addChild(child.asInstanceOf[TreeNode_TYPE])
  }

  override def addContent(aContent: E): TreeNode_TYPE = {
    val node = addChild()
    node.content = aContent
    node
  }

  protected final def set_child(child: TreeNode[E]): TreeNode_TYPE = {
    fill_children()
    require(Strings.notblankp(child.name), "TreeNodeStructureBase: empty name = " + pathname)
    require(getChild(child.name).isEmpty, "TreeNodeStructureBase: duplication = " + pathname + ", "  + child.name)
    child.setModified()
    child.parent = this.asInstanceOf[child.TreeNode_TYPE]
    child.facade = node_facade;
    node_children += child
    set_modified()
    child.asInstanceOf[TreeNode_TYPE]
  }

  protected def set_Child(child: TreeNode[E]): Unit = Unit

  protected def new_Node(name: String): TreeNode_TYPE

  protected def copy_Node(): TreeNode_TYPE = sys.error("missing copy_Node for deepCopy: " + this)

  override def removeChild(aChild: TreeNode[E]) {
    node_children -= aChild.asInstanceOf[TreeNode_TYPE]
  }

  override def getNode(pathname: String): Option[TreeNode_TYPE] =
    getNode(new PathName(pathname))

  override def getNode(pathname: PathName): Option[TreeNode_TYPE] = {
    var child: TreeNode[E] = this
    for (comp <- pathname.components) {
      val mayChild = child.getChild(comp)
      if (mayChild.isEmpty) {
	return mayChild.asInstanceOf[Option[TreeNode_TYPE]]
      }
      child = mayChild.get
    }
    new Some(child).asInstanceOf[Option[TreeNode_TYPE]]
  }

  override def setNode(pathname: String): TreeNode_TYPE =
    setNode(new PathName(pathname))

  override def setNode(pathname: PathName): TreeNode_TYPE = {
    var current: TreeNode[E] = this
    for (comp <- pathname.components) {
      val mayNode = current.getChild(comp)
      current = mayNode.getOrElse(current.setChild(comp))
    }
    set_modified()
    current.asInstanceOf[TreeNode_TYPE]
  }

  override def setContent(pathname: String, content: E): TreeNode_TYPE = {
    setContent(new PathName(pathname), content)
  }

  override def setContent(pathname: PathName, content: E): TreeNode_TYPE = {
    val node = setNode(pathname)
    node.content = content
    set_modified()
    node
  }

  def mergeCloneTree(tree: Tree[E]): TreeNode_TYPE = {
    val node = tree.root.deepCopy
    addChildren(node.children)
    this.asInstanceOf[TreeNode_TYPE]
  }

  def mergeCloneTree(pathname: String, tree: Tree[E]): TreeNode_TYPE =
    mergeCloneTree(PathName(pathname), tree)

  def mergeCloneTree(pathname: PathName, tree: Tree[E]): TreeNode_TYPE = {
    val a = Tree.mergeClone(this, pathname.v, tree.root).asInstanceOf[TreeNode_TYPE]
    node_children.clear()
    node_children ++= a.children
    this.asInstanceOf[TreeNode_TYPE]
  }

  override def children: Seq[TreeNode_TYPE] = {
    fill_children()
    node_children.toList.asInstanceOf[Seq[TreeNode_TYPE]]
  }

  private def fill_children(): Unit = {
    if (!is_filled_children) {
      for (child <- load_Children()) {
	child.parent = this.asInstanceOf[child.TreeNode_TYPE]
	node_children += child.asInstanceOf[TreeNode_TYPE]
      }
    }
    is_filled_children = true
  }

  protected def load_Children(): Seq[TreeNode_TYPE] = Nil

  final def clear() {
    node_children.clear
  }

  override def indexOf(aNode: TreeNode[E]): Int = {
    node_children.indexOf(aNode)
  }

  override def traverse(visitor: TreeVisitor[E]) {
    traverse(visitor, null)
  }

  override def traverse(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean) {
    if (filter != null && !filter.apply(this)) return
    visitor.start(this)
    if (is_stop_start(visitor, this)) return
    traverseChildren(visitor, filter)
    visitor.end(this)
  }

  override def traverseChildren(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean) {
    val childs = children
    if (childs.length > 0) {
      var child = childs(0)
      child.traverseNode(visitor, filter)
      for (i <- 1 until childs.length) {
	val prev = child;
	child = childs(i)
	visitor.stay(this, i, prev, child)
	if (is_stop_stay(visitor, this, i, prev, child)) return
	child.traverseNode(visitor, filter)
      }
    }
  }

  override def traverseNode(visitor: TreeVisitor[E], filter: TreeNode[E] => Boolean) {
    if (filter != null && !filter.apply(this)) return
    visitor.enter(this)
    if (is_stop_enter(visitor, this)) return
    traverseChildren(visitor, filter)
    visitor.leave(this)
  }

  final def traverse(aFunction: E => Unit) {
    traverse(new FunctionVisitor[E](aFunction))
  }

  private def is_stop_start(visitor: TreeVisitor[E], aNode: TreeNode[E]): Boolean = {
    visitor.isDone(aNode)
  }

  private def is_stop_enter(visitor: TreeVisitor[E], aNode: TreeNode[E]): Boolean = {
    visitor.isDone(aNode)
  }

  private def is_stop_stay(visitor: TreeVisitor[E], node: TreeNode[E], index: Int, prev: TreeNode[E], next: TreeNode[E]): Boolean = {
    visitor.isDone(node)
  }

  final def isModified: Boolean = is_modified

  final def clearModified() {
    is_modified = false
  }

  final def setModified() {
    set_modified()
  }

  private def set_modified() {
    is_modified = true
    if (node_facade != null) node_facade.setModified()
  }

  final def cursor: TreeCursor[E] = {
    new TreeCursor[E](this)
  }

  def deepCopy: TreeNode_TYPE = {
    val node = copy_Node()
    if (is_filled_children) 
      for (child <- node_children)
	node.addChild(child.deepCopy.asInstanceOf[node.TreeNode_TYPE])
    val node2 = node.asInstanceOf[TreeNodeStructureBase[E]]
    node2.is_filled_children = is_filled_children
    node2.is_modified = false
    node
  }

  //
  final def toText: String = {
    val buf = new StringBuilder
    buildText(buf)
    buf.toString
  }

  final def buildText(aBuffer: StringBuilder): StringBuilder = {
    build_Text(aBuffer)
    aBuffer
  }

  protected def build_Text(aBuffer: StringBuilder): StringBuilder = {
    build_Text_Prologue(aBuffer)
    children.foreach(_.buildText(aBuffer))
    build_Text_Epilogue(aBuffer)
    aBuffer
  }

  protected def build_Text_Prologue(aBuffer: StringBuilder): Unit = Unit
  protected def build_Text_Epilogue(aBuffer: StringBuilder): Unit = Unit

  // XML
  final def getXmlAttribute(key: String): Option[Any] = {
    xml_attributes.get(key)
  }

  final def getXmlAttributeString(key: String): Option[String] = {
    val mayValue = getXmlAttribute(key)
    if (mayValue == None) return None
    mayValue.get match {
      case v: String => mayValue.asInstanceOf[Option[String]]
      case v: Text => Some(v.toString)
      case v: PCData => Some(v.toString)
      case v => Some(v.toString)
    }
  }

  final def putXmlAttribute(key: String, value: Any): Option[Any] = {
    xml_attributes.put(key, value)
  }

  final def putXmlAttributes(theAttrs: MetaData) {
    for (attr <- theAttrs) {
      xml_attributes.put(attr.key, attr.value)
    }
  }

  final def getXmlLang: Option[String] = {
    xml_attributes.get("xml:lang").asInstanceOf[Option[String]]
  }

  final def getXmlLocale: Option[Locale] = {
    xml_attributes.get("locale").asInstanceOf[Option[Locale]]
  }

  final def toXml: Node = {
    var node = xml_Node()
    if (node != null) return node
    new Elem(make_prefix, make_element_label, make_attributes, TopScope, true, make_children: _*)
  }

  final def toPrettyXml: String = {
    val printer = new PrettyPrinter(80, 1)
    printer.format(toXml)
  }

  protected final def make_prefix: String = null // XXX

  protected final def make_element_label: String = {
    if (xml_Element_Label != null) xml_Element_Label
    else getClass.getSimpleName
  }

  protected final def make_attributes: MetaData = {
    xml_attributes.toMetaData(
      if (xml_Element_Label != null) xml_Attributes
      else if (name != null) Array((null, "name", name),
				   (null, "content", content))
      else Nil)
  }

  protected final def make_children: Seq[Node] = {
    children.map(_.toXml)
  }

  protected def xml_Node(): Node = null
  protected def xml_Element_Label: String = null
  protected def xml_Element_Uri: String = null
  protected def xml_Attributes: Seq[(String, String, Any)] = Nil
}
