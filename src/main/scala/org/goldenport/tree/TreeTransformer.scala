package org.goldenport.tree

import scala.util.matching.Regex
import org.goldenport.RAISE
import org.goldenport.value._
import org.goldenport.util.CirceUtils.Codec._
import org.goldenport.util.RegexUtils

/*
 * @since   Nov. 14, 2020
 *  version Nov. 15, 2020
 *  version Jan.  1, 2021
 *  version Mar. 31, 2025
 *  version Apr. 27, 2025
 *  version May. 31, 2025
 * @version Jun.  4, 2025
 * @author  ASAMI, Tomoharu
 */
trait TreeTransformer[A, B] {
  import TreeTransformer._

  def isEndomap: Boolean = false
  def isCleanVoid: Boolean = true
  def treeTransformerContext: Context[B]
  def rule: Rule[A, B] = Rule.default[A, B]

  private lazy val _factory = treeTransformerContext.factory
  private def _config = rule.config orElse treeTransformerContext.config getOrElse Config.default

  def apply(p: Tree[A]): Tree[B] = apply(p.root)

  def apply(p: TreeNode[A]): Tree[B] = {
    val a = make_tree_node(p)
    a match {
      case Nil => Tree.create[B]
      case x :: xs => create_tree(x)
    }
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

  protected def make_node(p: TreeNode[A]): Directive[B] = {
    _config.scope.policy match {
      case Config.Scope.Policy.All =>
        if (_config.scope.isExclude(p))
          Directive.Empty()
        else
          _make_node(p)
      case Config.Scope.Policy.HomeOnly =>
        if (_config.scope.isExclude(p))
          Directive.Empty()
        else if (_config.scope.isInclude(p))
          _make_node(p)
        else
          p.pathList.length match {
            case 0 => _make_node(p)
            case 1 if p.isLeaf => _make_node(p)
            case _ => Directive.Empty()
          }
      case Config.Scope.Policy.ExcludeHome =>
        if (_config.scope.isExclude(p))
          Directive.Empty()
        else if (_config.scope.isInclude(p))
          _make_node(p)
        else
          p.pathList.length match {
            case 0 => _make_node(p)
            case 1 =>
              if (p.isContainer)
                _make_node(p)
              else
                Directive.Empty()
            case _ => _make_node(p)
          }
    }
  }

  private def _make_node(p: TreeNode[A]): Directive[B] =
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

  protected final def directive_empty(): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.Empty()

  protected final def directive_default(): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.Default()

  protected final def directive_asis(): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.AsIs()

  protected final def directive_leaf(p: B): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.LeafContent(p)

  protected final def directive_leaf(name: String, p: B): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.LeafNode(name, p)

  protected final def directive_node(name: String, p: B): TreeTransformer.Directive[B] =
    TreeTransformer.Directive.Node(TreeNode.create(name, p))
}

object TreeTransformer {
  case class Context[E](
    config: Option[Config] = None,
    factory: TreeFactory[E]
  )
  object Context {
    private val _default = Context(factory = TreeFactory.default)
    def default[E] = _default.asInstanceOf[Context[E]]
  }

  case class Config(
    scope: Config.Scope = Config.Scope.all
  )
  object Config {
    import io.circe._
    import io.circe.generic.extras._
    import io.circe.generic.extras.semiauto._

    val default = Config()

    implicit val circeconf = Configuration.default.
      withDefaults.withSnakeCaseMemberNames

    case class Scope(
      policy: Scope.Policy = Scope.Policy.All,
      includePaths: List[Regex] = Nil,
      excludePaths: List[Regex] = Nil
    ) {
      def isInclude[A](p: TreeNode[A]): Boolean =
        RegexUtils.isWholeMatch(includePaths, p.pathname)

      def isExclude[A](p: TreeNode[A]): Boolean =
        RegexUtils.isWholeMatch(excludePaths, p.pathname)
    }
    object Scope {
      val all = Scope()

      sealed trait Policy extends NamedValueInstance {
      }
      object Policy extends EnumerationClass[Policy] {
        val elements = Vector(All, HomeOnly, ExcludeHome)

        case object All extends Policy {
          def name = "all"
        }
        case object HomeOnly extends Policy {
          def name = "home_only"
        }
        case object ExcludeHome extends Policy {
          def name = "exclude_home"
        }

        def create(p: String): Either[String, Policy] =
          elements.find(_.name == p).map(Right(_)) getOrElse {
            Left(s"Unknown Policy: $p")
          }

        implicit val policyDecoder: Decoder[Policy] = Decoder.decodeString.emap(create)
        implicit val policyEncoder: Encoder[Policy] = Encoder.encodeString.contramap(_.name)
      }
    }

    implicit val scopedecoder: Decoder[Scope] = deriveConfiguredDecoder
    implicit val scopeencoder: Encoder[Scope] = deriveConfiguredEncoder

    implicit val configdecoder: Decoder[Config] = deriveConfiguredDecoder
    implicit val configencoder: Encoder[Config] = deriveConfiguredEncoder
  }

  trait Rule[A, B] {
    def config: Option[Config] = None
    def getTargetName(p: TreeNode[A]): Option[String] = None
    def makeContent(p: A): Option[B] = None
    def makeContent(oldname: String, newname: String, p: A): Option[B] = None
//    def mapContent(oldname: String, newname: String, p: A): B = RAISE.noReachDefect(s"$oldname -> $newname: $p")
  }
  object Rule {
    case class AsIs[A, B]() extends Rule[A, B] {
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
