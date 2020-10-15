package org.goldenport.realm

import java.io.File
import java.nio.charset.Charset
import java.net.URL
import org.goldenport.cli
import org.goldenport.tree._
import org.goldenport.bag.Bag
import org.goldenport.io.IoUtils
import org.goldenport.values.PathName

/*
 * @since   Dec. 12, 2019
 *  version Dec. 15, 2019
 *  version Mar.  1, 2020
 *  version May.  4, 2020
 *  version Jul. 13, 2020
 * @version Oct. 11, 2020
 * @author  ASAMI, Tomoharu
 */
case class Realm(
  private val _tree: Tree[Realm.Data]
) {
  import Realm._

  def get(pathname: String): Option[Data] = _tree.getContent(pathname)

  def traverse(p: TreeVisitor[Realm.Data]):  Unit = _tree.traverse(p)

  def export(ctx: Context) {
    val dir = ctx.outputDirectory
    _export(dir, _tree.root)(ctx)
  }

  private def _export(dir: File, node: TreeNode[Data])(implicit ctx: Context) {
    for (c <- node.children) {
      if (c.isLeaf) {
        c.content.export(new File(dir, c.name))
      } else {
        _export(new File(dir, c.name), c)
      }
    }
  }

  def merge(pathname: String, view: Realm): Realm = ???
}

object Realm {
  def createEmpty() = Realm(new PlainTree())

  sealed trait Data {
    def export(file: File)(implicit ctx: Context): Unit
  }

  case class StringData(string: String) extends Data {
    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, string, ctx.charset)
  }

  case class UrlData(url: URL) extends Data {
    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, url)
  }

  case class BagData(bag: Bag) extends Data {
    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, bag)
  }

  trait ApplicationData extends Data {
  }

  trait StringApplicationData extends ApplicationData {
    def marshall: String

    def export(file: File)(implicit ctx: Context): Unit = {
      val string = marshall
      IoUtils.save(file, string, ctx.charset)
    }
  }

  trait BinaryApplicationData extends ApplicationData {
    def marshall: Bag

    def export(file: File)(implicit ctx: Context): Unit = {
      val bag: Bag = marshall
      IoUtils.save(file, bag)
    }
  }

  case class Config(cliConfig: cli.Config) {
    def outputDirectory: File = cliConfig.outputDirectory
    def charset: Charset = cliConfig.charset
  }

  case class Context(config: Config) {
    def outputDirectory: File = config.outputDirectory
    def charset: Charset = config.charset
  }
  object Context {
    def create(cliconfig: cli.Config) = Context(Config(cliconfig))
  }

  class Builder() {
    private val _tree: Tree[Data] = new PlainTree[Data]()

    def set(pathname: PathName, string: String): Builder = {
      _tree.setContent(pathname, StringData(string))
      this
    }

    def set(pathname: PathName, url: URL): Builder = {
      _tree.setContent(pathname, UrlData(url))
      this
    }

    def set(pathname: PathName, bag: Bag): Builder = {
      _tree.setContent(pathname, BagData(bag))
      this
    }

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

  trait Visitor extends TreeVisitor[Realm.Data] {
  }

  case class View(root: TreeNode[Realm.Data]) {
  }
  object View {
    val empty = View(TreeNode.empty())
  }
}
