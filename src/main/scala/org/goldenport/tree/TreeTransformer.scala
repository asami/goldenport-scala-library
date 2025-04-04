package org.goldenport.tree

import org.goldenport.RAISE

/*
 * @since   Nov. 14, 2020
 *  version Nov. 15, 2020
 *  version Jan.  1, 2021
 *  version Mar. 31, 2025
 * @version Apr.  3, 2025
 * @author  ASAMI, Tomoharu
 */
trait TreeTransformer[A, B] {
  import TreeTransformer._

  def isEndomap: Boolean = false
  def isCleanVoid: Boolean = true
  def treeTransformerContext: Context[B]
  def rule: Rule[A, B] = Rule.default[A, B]

  lazy val _factory = treeTransformerContext.factory

  def apply(p: Tree[A]): Tree[B] = apply(p.root)

  def apply(p: TreeNode[A]): Tree[B] = {
    val a = make_tree_node(p).head
    create_tree(a)
  }

  protected final def create_tree(node: TreeNode[B]): Tree[B] =
    _factory.createTree(node: TreeNode[B])

  protected final def create_tree_node(name: String, content: Option[B], children: Seq[TreeNode[B]]): TreeNode[B] =
    _factory.createTreeNode(name, content, children)

  protected final def create_tree_node(name: String, content: B, children: Seq[TreeNode[B]]): TreeNode[B] =
    _factory.createTreeNode(name, content, children)

  protected def make_tree_node(p: TreeNode[A]): List[TreeNode[B]] =
    rule.getTargetName(p).map { name =>
      // println(s"make_tree_node: $name")
      make_node_or_control(p.name, name, p)
    }.getOrElse {
      p.getContent.fold {
        if (p.children.isEmpty) {
          // println(s"make_tree_node Nil: ${p.name}")
          Nil
        } else {
          // println(s"make_tree_node A: ${p.name}")
          make_node_or_control(p)
        }
      } { x =>
        // println(s"make_tree_node B: ${p.name}")
        make_node_or_control(p)
      }
    }

  protected def make_node_or_control(oldname: String, newname: String, p: TreeNode[A]): List[TreeNode[B]] = {
    val a: Directive[B] = make_node(oldname, newname, p)
    a match {
      case Directive.Empty() => Nil
      case Directive.AsIs() => List(p.asInstanceOf[TreeNode[B]])
      case Directive.Default() =>
        p.getContent match {
          case Some(s) => 
            val r = _create_node(oldname, newname, p, s)
            List(r)
          case None => 
            if (p.isContainer) {
              val r = _create_node(oldname, newname, p)
              List(r)
            } else {
              Nil
            }
        }
      case m: Directive.LeafContent[B] => List(_create_leaf(p, m.content))
      case m: Directive.Content[B] => List(_create_node(p, m.content))
      case m: Directive.LeafNode[B] => List(_create_leaf(m.name, m.content))
      case m: Directive.NameNode[B] => List(_create_node_children(m.name, m.content, m.children))
      case m: Directive.Node[B] => m.node match {
        case ControlTreeNode.Empty() => Nil
        case ControlTreeNode.AsIs(mm) => List(mm)
        case ControlTreeNode.Collection(ms) => ms
        case mm => List(mm)
      }
      case Directive.Nodes(nodes) => nodes
    }
  }

  protected def make_node(oldname: String, newname: String, p: TreeNode[A]): Directive[B] =
    p.getContent.fold {
      // println(s"a: $p")
      make_Node(oldname, newname, p)
    } { x =>
      // println(s"b: $p, $x")
      make_Node(oldname, newname, p, x)
    }

  // protected def make_node0(oldname: String, newname: String, p: TreeNode[A]): Directive[B] = {
  //   val a = p.getContent.fold {
  //     println(s"a: $p")
  //     make_Node(oldname, newname, p)
  //   } { x =>
  //     println(s"b: $p, $x")
  //     make_Node(oldname, newname, p, x)
  //   }
  //   a getOrElse {
  //     if (p.isContainer) {
  //       val r = _create_node(oldname, newname, p)
  //       Some(r)
  //     } else {
  //       None
  //     }
  //   }
  // }

  protected def make_Node(oldname: String, newname: String, node: TreeNode[A]): Directive[B] = make_Node(node)

  protected def make_Node(oldname: String, newname: String, node: TreeNode[A], content: A): Directive[B] = make_Node(node)

  private def _create_node(oldname: String, newname: String, p: TreeNode[A], content: A): TreeNode[B] = {
    val xs = p.children.flatMap(make_tree_node)
    val c: Option[B] = make_content(oldname, newname, content)
    create_tree_node(newname, c, xs)
  }

  private def _create_node(oldname: String, newname: String, p: TreeNode[A]): TreeNode[B] = {
    val xs = p.children.flatMap(make_tree_node)
    create_tree_node(newname, None, xs)
  }

  protected def make_content(oldname: String, newname: String, p: A): Option[B] =
    make_Content(oldname, newname, p) orElse rule.makeContent(oldname, newname, p) orElse make_default_content(p)

  protected def make_Content(oldname: String, newname: String, p: A): Option[B] = None

  protected def make_node_or_control(p: TreeNode[A]): List[TreeNode[B]] = {
    make_node(p) match {
      case Directive.Empty() => Nil
      case Directive.AsIs() => List(p.asInstanceOf[TreeNode[B]])
      case Directive.Default() =>
        if (p.isVoid && isCleanVoid) {
          Nil
        } else {
          val r = _create_node(p)
          List(r)
        }
      case m: Directive.LeafContent[B] => List(_create_leaf(p, m.content))
      case m: Directive.Content[B] => List(_create_node(p, m.content))
      case m: Directive.LeafNode[B] => List(_create_leaf(m.name, m.content))
      case m: Directive.NameNode[B] => List(_create_node_children(m.name, m.content, m.children))
      case m: Directive.Node[B] => m.node match {
        case ControlTreeNode.Empty() => Nil
        case ControlTreeNode.AsIs(mm) => List(mm)
        case ControlTreeNode.Collection(ms) => ms
        case mm => List(mm)
      }
      case Directive.Nodes(nodes) => nodes
    }
  }

  //     case ControlTreeNode.Empty() => Nil
  //     case ControlTreeNode.AsIs(m) => List(m)
  //     case ControlTreeNode.Collection(ms) => ms
  //     case m => List(m)
  //   }.getOrElse {
  //     val r = _create_node(p)
  //     List(r)
  //   }
  // }

  protected def make_node(p: TreeNode[A]): Directive[B] =
    p.getContent.fold {
      // println(s"a: $p")
      make_Node(p)
    } { x =>
      // println(s"b: $p, $x")
      make_Node(p, x)
    }

  // protected def make_node0(p: TreeNode[A]): Option[TreeNode[B]] = {
  //   val a = p.getContent.fold {
  //     println(s"a: $p")
  //     make_Node(p)
  //   } { x =>
  //     println(s"b: $p, $x")
  //     make_Node(p, x)
  //   }
  //   a orElse {
  //     if (p.isContainer) {
  //       val r = _create_node(p)
  //       Some(r)
  //     } else {
  //       None
  //     }
  //   }
  // }


  private def _create_leaf(p: TreeNode[A], content: B): TreeNode[B] =
    create_tree_node(p.name, Some(content), Nil)

  private def _create_leaf(name: String, content: B): TreeNode[B] =
    create_tree_node(name, Some(content), Nil)

  private def _create_node(p: TreeNode[A]): TreeNode[B] = {
    val xs = p.children.flatMap(make_tree_node)
    val c: Option[B] = make_content(p.content)
    create_tree_node(p.name, c, xs)
  }

  private def _create_node(p: TreeNode[A], content: B): TreeNode[B] = {
    val xs = p.children.toList.flatMap(make_tree_node)
    create_tree_node(p.name, Some(content), xs)
  }

  private def _create_node(name: String, content: B, children: Seq[TreeNode[A]]): TreeNode[B] = {
    val xs = children.toList.flatMap(make_tree_node)
    create_tree_node(name, Some(content), xs)
  }

  private def _create_node_children(name: String, content: B, children: Seq[TreeNode[B]]): TreeNode[B] = {
    create_tree_node(name, Some(content), children)
  }

  protected def make_Node(node: TreeNode[A]): Directive[B] = Directive.Default[B]

  protected def make_Node(node: TreeNode[A], content: A): Directive[B] = Directive.Default[B]

  protected def make_content(p: A): Option[B] =
    make_Content(p) orElse rule.makeContent(p) orElse make_default_content(p)

  protected def make_Content(p: A): Option[B] = None

  protected def make_default_content(p: A): Option[B] =
    if (isEndomap)
      Some(p.asInstanceOf[B])
    else
      None
}

object TreeTransformer {
  case class Context[E](
//    context: Context,
    factory: TreeFactory[E]
  )
  object Context {
    private val _default = Context(TreeFactory.default)
    def default[E] = _default.asInstanceOf[Context[E]]
  }

  trait Rule[A, B] {
    def getTargetName(p: TreeNode[A]): Option[String]
    def makeContent(p: A): Option[B] = None
    def makeContent(oldname: String, newname: String, p: A): Option[B] = None
//    def mapContent(oldname: String, newname: String, p: A): B = RAISE.noReachDefect(s"$oldname -> $newname: $p")
  }
  object Rule {
    case class AsIs[A, B]() extends Rule[A, B] {
      def getTargetName(p: TreeNode[A]): Option[String] = None
      override def makeContent(oldname: String, newname: String, p: A): Option[B] = Some(p.asInstanceOf[B])
    }

    private val _default = AsIs()
    def default[A, B] = _default.asInstanceOf[Rule[A, B]]
  }

  sealed trait Directive[T] {
  }
  object Directive {
    case class Empty[T]() extends Directive[T]
    case class AsIs[T]() extends Directive[T]
    case class Default[T]() extends Directive[T]
    case class LeafContent[T](content: T) extends Directive[T]
    case class Content[T](content: T) extends Directive[T]
    case class LeafNode[T](name: String, content: T) extends Directive[T]
    case class NameNode[T](name: String, content: T, children: List[TreeNode[T]]) extends Directive[T]
    case class Node[T](node: TreeNode[T]) extends Directive[T]
    object Node {
      def create[T](p: T): Node[T] = Node(TreeNode.createContentNode(p))
    }
    case class Nodes[T](nodes: List[TreeNode[T]]) extends Directive[T]
    object Nodes {
      def create[T](ps: Seq[T]): Nodes[T] = Nodes(ps.map(TreeNode.createContentNode(_)).toList)
    }
  }
}
