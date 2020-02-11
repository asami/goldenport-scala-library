package org.goldenport.realm

import java.io.File
import java.net.URL
import org.goldenport.tree._
import org.goldenport.bag.Bag

/*
 * @since   Dec. 12, 2019
 * @version Dec. 15, 2019
 * @author  ASAMI, Tomoharu
 */
case class Realm(
  private val _tree: Tree[Realm.Data]
) {
  def export(target: File) {
    ???
  }
}

object Realm {
  sealed trait Data {
  }

  case class StringData(string: String) extends Data {
  }

  case class UrlData(url: URL) extends Data {
  }

  case class BagData(bag: Bag) extends Data {
  }

  class Builder() {
    private val _tree: Tree[Data] = new PlainTree[Data]()

    def set(pathname: String, string: String): Builder = {
      _tree.setContent(pathname, StringData(string))
      this
    }

    def set(pathname: String, url: URL): Builder = {
      _tree.setContent(pathname, UrlData(url))
      this
    }

    def set(pathname: String, bag: Bag): Builder = {
      _tree.setContent(pathname, BagData(bag))
      this
    }

    def build: Realm = Realm(_tree)
  }
  object Builder {
    def apply(): Builder = new Builder()
  }
}
