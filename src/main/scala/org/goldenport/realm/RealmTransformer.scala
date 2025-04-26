package org.goldenport.realm

// import scala.collection.mutable.ArrayBuffer
import org.goldenport.tree._

/*
 * @since   Nov. 15, 2020
 *  version Jan.  1, 2021
 *  version Mar. 18, 2025
 * @version Apr. 23, 2025
 * @author  ASAMI, Tomoharu
 */
trait RealmTransformer extends HomoTreeTransformer[Realm.Data] with RealmMaker.Helper[Realm.Data] {
  def realmTransformerContext: RealmTransformer.Context
  def treeTransformerContext = realmTransformerContext.treeTransformerContext

  // protected final def directive_empty(): TreeTransformer.Directive[Realm.Data] =
  //   TreeTransformer.Directive.Empty()

  // protected final def directive_default(): TreeTransformer.Directive[Realm.Data] =
  //   TreeTransformer.Directive.Default()

  // protected final def directive_asis(): TreeTransformer.Directive[Realm.Data] =
  //   TreeTransformer.Directive.AsIs()

  // protected final def directive_leaf(p: Realm.Data): TreeTransformer.Directive[Realm.Data] =
  //   TreeTransformer.Directive.LeafContent(p)

  // protected final def directive_node(name: String, p: Realm.Data): TreeTransformer.Directive[Realm.Data] =
  //   TreeTransformer.Directive.Node(TreeNode.create(name, p))
}

object RealmTransformer {
  trait Rule extends TreeTransformer.Rule[Realm.Data, Realm.Data] {
  }
  object Rule {
    def default = TreeTransformer.Rule.default[Realm.Data, Realm.Data]
  }

  case class Context(
    treeTransformerContext: TreeTransformer.Context[Realm.Data] = TreeTransformer.Context.default[Realm.Data]
  )
  object Context {
    def default = Context()
  }
}
