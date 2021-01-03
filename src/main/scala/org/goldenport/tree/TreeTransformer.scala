package org.goldenport.tree

import org.goldenport.RAISE

/*
 * @since   Nov. 14, 2020
 *  version Nov. 15, 2020
 * @version Jan.  1, 2021
 * @author  ASAMI, Tomoharu
 */
trait TreeTransformer[A, B] {
  import TreeTransformer._

  def treeTransformerContext: Context[B]
  def rule: Rule[A, B]

  lazy val _factory = treeTransformerContext.factory

  def apply(p: Tree[A]): Tree[B] = create_tree(apply(p.root))

  def apply(p: TreeNode[A]): TreeNode[B] =
    make_tree_node(p).getOrElse(RAISE.noReachDefect)

  protected final def create_tree(node: TreeNode[B]): Tree[B] =
    _factory.createTree(node: TreeNode[B])

  protected final def create_tree_node(name: String, content: B, children: Seq[TreeNode[B]]): TreeNode[B] =
    _factory.createTreeNode(name, content, children)

  protected def make_tree_node(p: TreeNode[A]): Option[TreeNode[B]] =
    rule.getTargetName(p).map { name =>
      val xs = p.children.flatMap(make_tree_node)
      val c = make_content(p.content)
      create_tree_node(name, c, xs)
    }.orElse {
      val xs = p.children.flatMap(make_tree_node)
      Some(create_tree_node(p.name, p.content.asInstanceOf[B], xs))
    }

  protected def make_content(p: A): B = make_Content(p) getOrElse rule.mapContent(p)

  protected def make_Content(p: A): Option[B] = None
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
    def mapContent(p: A): B
  }
  object Rule {
    case class AsIs[A, B]() extends Rule[A, B] {
      def getTargetName(p: TreeNode[A]): Option[String] = Some(p.name)
      def mapContent(p: A): B = p.asInstanceOf[B]
    }

    private val _default = AsIs()
    def default[A, B] = _default.asInstanceOf[Rule[A, B]]
  }
}
