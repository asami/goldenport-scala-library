package org.goldenport.realm

// import scala.collection.mutable.ArrayBuffer
import org.goldenport.tree._

/*
 * @since   Nov. 15, 2020
 * @version Jan.  1, 2021
 * @author  ASAMI, Tomoharu
 */
trait RealmTransformer extends TreeTransformer[Realm.Data, Realm.Data] {
//  def apply(p: Realm): Realm = p.transform(this)
}

object RealmTransformer {
  trait Rule extends TreeTransformer.Rule[Realm.Data, Realm.Data] {
    
  }

  object Context {
    def default = TreeTransformer.Context.default[Realm.Data]
  }
  object Rule {
    def default = TreeTransformer.Rule.default[Realm.Data, Realm.Data]
  }
}
