package org.goldenport.realm

import org.goldenport.tree._

/*
 * @since   Apr. 23, 2025
 * @version Apr. 25, 2025
 * @author  ASAMI, Tomoharu
 */
object RealmMaker {
  trait Helper[A] { self :TreeTransformer[A, Realm.Data] =>
    // protected final def directive_empty(): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.Empty()

    // protected final def directive_default(): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.Default()

    // protected final def directive_asis(): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.AsIs()

    // protected final def directive_leaf(p: Realm.Data): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.LeafContent(p)

    // protected final def directive_leaf(name: String, p: Realm.Data): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.LeafNode(name, p)

    // protected final def directive_node(name: String, p: Realm.Data): TreeTransformer.Directive[Realm.Data] =
    //   TreeTransformer.Directive.Node(TreeNode.create(name, p))
  }

  trait Transformer[A] extends TreeTransformer[A, Realm.Data] with Helper[A] {
  }

  def make[A](in: Tree[A], tx: Transformer[A]): Realm = {
    val a = in.transform(tx)
    Realm(a)
  }
}
