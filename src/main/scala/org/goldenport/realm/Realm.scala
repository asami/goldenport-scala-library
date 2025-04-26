package org.goldenport.realm

import scalaz.{Tree => ZTree}
import scala.util.Try
import scala.util.matching.Regex
import java.io.File
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.nio.charset.Charset
import java.nio.file.Path
import java.net.URL
import org.goldenport.RAISE
import org.goldenport.Platform
import org.goldenport.context.Showable
import org.goldenport.value._
import org.goldenport.cli
import org.goldenport.tree._
import org.goldenport.bag.Bag
import org.goldenport.io.InputSource
import org.goldenport.io.IoUtils
import org.goldenport.values.PathName
import org.goldenport.util.StringUtils
import org.goldenport.util.RegexUtils

/*
 * @since   Dec. 12, 2019
 *  version Dec. 15, 2019
 *  version Mar.  1, 2020
 *  version May.  4, 2020
 *  version Jul. 13, 2020
 *  version Oct. 11, 2020
 *  version Nov. 15, 2020
 *  version Jan.  4, 2021
 *  version Mar. 19, 2022
 *  version Feb. 25, 2025
 *  version Mar. 30, 2025
 * @version Apr. 25, 2025
 * @author  ASAMI, Tomoharu
 */
case class Realm(
  private val _tree: Tree[Realm.Data],
  origin: Option[File] = None
) extends Showable {
  import Realm._

  def print: String = _tree.print
  override def display: String = _tree.display
  override def show: String = _tree.show

  def backend: Tree[Data] = _tree

  def backendRoot: TreeNode[Data] = _tree.root

  def get(pathname: String): Option[Data] = _tree.getContent(pathname)

  def tree: ZTree[NodeView] = _tree.ztree.map(NodeView.apply)

  def traverse(p: Visitor): Unit = _tree.traverse(p)

  def transform(p: RealmTransformer): Realm = Realm(_tree.transform(p))

  def transformTree[A](p: TreeTransformer[Data, A]): Tree[A] = _tree.transform(p)

  def export(dir: File) {
    val ctx = Context(dir)
    export(ctx)
  }

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

  /*
   * Mutation
   */
  // complement
  def merge(pathname: String, view: Realm): Realm = Realm(Tree.mergeClone(this._tree, pathname, view._tree))

  def +(p: Realm): Realm = Realm(Tree.mergeClone(this._tree, p._tree))

  def setNode(pathname: PathName): Realm = {
    _tree.setNode(pathname.v)
    this
  }

  def setNode(pathname: String): Realm = {
    _tree.setNode(pathname)
    this
  }

  def setContent(pathname: PathName, string: String): Realm = {
    _tree.setContent(pathname, StringData(string))
    this
  }

  def setContent(pathname: String, string: String): Realm = {
    _tree.setContent(pathname, StringData(string))
    this
  }

  def getCursor(pathname: PathName): Option[Cursor] =
    _tree.getNode(pathname.v).map(Cursor.create)

  def getCursor(pathname: String): Option[Cursor] =
    _tree.getNode(pathname).map(Cursor.create)

  def takeCursor(pathname: PathName): Cursor =
    getCursor(pathname) getOrElse RAISE.missingPropertyFault(pathname.v)

  def takeCursor(pathname: String): Cursor =
    getCursor(pathname) getOrElse RAISE.missingPropertyFault(pathname)
}

object Realm {
  sealed trait ExportStrategy extends NamedValueInstance {
  }
  object ExportStrategy extends EnumerationClass[ExportStrategy] {
    val elements = Vector(Overwrite, Update)

    case object Overwrite extends ExportStrategy {
      val name = "overwrite"
    }
    case object Update extends ExportStrategy {
      val name = "update"
    }
  }

  sealed trait Data extends Showable {
    def export(file: File)(implicit ctx: Context): Unit

    // TODO goldenport.io.Exporter
    protected final def export_string(
      file: File,
      string: String
    )(implicit ctx: Context): Unit = ctx.exportStrategy match {
      case ExportStrategy.Overwrite => IoUtils.save(file, string, ctx.charset)
      case ExportStrategy.Update => _export_update(file, InputSource(string))
    }

    protected final def export_url(
      file: File,
      url: URL
    )(implicit ctx: Context): Unit = ctx.exportStrategy match {
      case ExportStrategy.Overwrite => IoUtils.save(file, url)
      case ExportStrategy.Update => _export_update(file, InputSource(url))
    }

    protected final def export_file(
      file: File,
      out: File
    )(implicit ctx: Context): Unit = ctx.exportStrategy match {
      case ExportStrategy.Overwrite => IoUtils.save(file, file)
      case ExportStrategy.Update => _export_update(file, InputSource(out))
    }

    protected final def export_bag(
      file: File,
      bag: Bag
    )(implicit ctx: Context): Unit = ctx.exportStrategy match {
      case ExportStrategy.Overwrite => IoUtils.save(file, bag)
      case ExportStrategy.Update => _export_update(file, InputSource(bag))
    }

    private def _export_update(file: File, in: InputSource) {
      if (_is_write(file, in))
        IoUtils.save(file, in)
    }

    private def _is_write(file: File, in: InputSource) = {
      if (file.exists) {
        !_is_same_close(new FileInputStream(file), in.openInputStream)
      } else {
        true
      }
    }

    import java.io.InputStream

    private def _is_same_close(lhs: InputStream, rhs: InputStream): Boolean = try {
      val ls = new BufferedInputStream(lhs)
      val rs = new BufferedInputStream(rhs)
      var byte1 = ls.read()
      var byte2 = rs.read()
      while (byte1 != -1 && byte2 != -1 && byte1 == byte2) {
        byte1 = ls.read()
        byte2 = rs.read()
      }
      ls.close()
      rs.close()
      byte1 == byte2
    } catch {
      case e: Throwable =>
        Try(lhs.close)
        Try(rhs.close)
        throw e
    }
  }

  case class StringData(string: String) extends Data {
    def print = string
    override def display = '"' + string + '"'
    override def show = s"String($string)"

    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, string, ctx.charset)
  }

  case class UrlData(url: URL) extends Data {
    def print = url.toString
    override def show = s"URL($url)"

    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, url)
  }

  case class FileData(file: File) extends Data {
    def print = show
    override def show = s"File($file)"

    def export(output: File)(implicit ctx: Context): Unit =
      IoUtils.save(output, file)
  }

  case class BagData(bag: Bag) extends Data {
    def print = show
    override def show = s"Bag(${bag.filename})"

    def export(file: File)(implicit ctx: Context): Unit =
      IoUtils.save(file, bag)
  }

  case class ObjectData(o: Object) extends Data {
    def print = Showable.toPrint(o)
    override def show = Showable.toShow(o)

    def export(file: File)(implicit ctx: Context): Unit = RAISE.unsupportedOperationFault(s"${o}")
  }

  case object EmptyData extends Data {
    def print = show
    override def show = "EMPTY"

    def export(file: File)(implicit ctx: Context): Unit = {}
  }

  sealed trait ApplicationData extends Data {
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

  case class Context(
    config: Config,
    outputDirectory: File,
    charset: Charset,
    exportStrategy: ExportStrategy = ExportStrategy.Update
  ) {
  }
  object Context {
    def apply(config: Config): Context =
      Context(
        config,
        config.outputDirectory,
        config.charset
      )

    def apply(outputDirectory: File): Context =
      apply(outputDirectory, Platform.charset.UTF8)

    def apply(outputDirectory: File, charset: Charset): Context =
      Context(Config(cli.Config.default), outputDirectory, charset)

    def create(cliconfig: cli.Config) = Context(Config(cliconfig))
  }

  case class Cursor(node: TreeNode[Data]) {
    def content = node.content

    def enter(name: String): Cursor = copy(node.setChild(name))

    def enterContent(content: Data): Cursor =
      copy(node.addContent(content))

    def leaveContent(content: Data): Cursor = {
      require (node.content == content)
      copy(node.parent)
    }

    def leave(): Cursor = copy(node.parent)

    def set(name: String, content: Data): Cursor =
      copy(node.setChild(name, content))

    def set(name: String, content: String): Cursor =
      set(name, StringData(content))

    def add(content: Data): Cursor = copy(node.addContent(content))

    def merge(pathname: String, realm: Realm): Cursor =
      merge(pathname, realm.backend)

    def merge(pathname: String, tree: Tree[Data]): Cursor = {
      node.mergeCloneTree(pathname, tree)
      this
    }
  }
  object Cursor {
    def create(node: TreeNode[Data]): Cursor = new Cursor(node)
  }

  class Builder(
    config: Builder.Config,
    var root: Option[File] = None
  ) {
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

    def setObject(pathname: String, o: Object): Builder = {
      _tree.setContent(pathname, ObjectData(o))
      this
    }

    def setup(root: File): Builder = {
      this.root = Some(root)
      val files = root.listFiles.toVector
      val c = this.cursor
      for (a <- files)
        _setup(c, a)
      this
    }

    private def _setup(cursor: TreeCursor[Realm.Data], p: File): Unit =
      if (p.isDirectory)
        _setup_directory(cursor, p)
      else
        _setup_file(cursor, p)

    private def _setup_directory(cursor: TreeCursor[Realm.Data], p: File): Unit = {
      cursor.enter(p.getName)
      val files = p.listFiles.toVector
      for (a <- files)
        _setup(cursor, a)
      cursor.leave()
    }

    private def _setup_file(cursor: TreeCursor[Realm.Data], p: File): Unit = {
      val name = p.getName.toLowerCase
      val suffix = StringUtils.toSuffix(name)
      if (_is_text(suffix))
        cursor.set(name, _to_text(p))
      else if (_is_binary(suffix))
        cursor.set(name, _to_binary(p))
      else if (_is_include(name))
        cursor.set(name, _to_binary(p))
      else if (_is_strict)
        Unit
      else if (!_is_exclude(name))
        cursor.set(name, _to_binary(p))
    }

    private def _is_text(suffix: String) = config.textSuffixes.contains(suffix)

    private def _is_binary(suffix: String) = config.binarySuffixes.contains(suffix)

    private def _is_strict = config.isStrict

    private def _is_include(filename: String) =
      config.includeFiles.exists(RegexUtils.isWholeMatch(_, filename))

    private def _is_exclude(filename: String) =
      config.excludeFiles.exists(RegexUtils.isWholeMatch(_, filename))

    private def _to_text(p: File) = StringData(IoUtils.toText(p))

    private def _to_binary(p: File) = FileData(p)

    def cursor: TreeCursor[Realm.Data] = _tree.cursor

    def build(): Realm = Realm(_tree, root)
  }
  object Builder {
    case class Config(
      isStrict: Boolean = false,
      textSuffixes: Set[String] = Set("dox", "md", "markdown", "org", "html", "jade", "pub", "ssp", "scaml", "mustache"),
      binarySuffixes: Set[String] = Set(
        "png", "jpg", "jpeg", "gif", "apng", "webp", "svg", "avif", "css", "js",
        "woff2", "woff", "ttf", "eof"
      ),
      includeFiles: Set[Regex] = Set(),
      excludeFiles: Set[Regex] = Set(
        ".*~$".r,
        ".*.bak$".r
      )
    )
    object Config {
      val default = Config()
    }

    def apply(): Builder = new Builder(Config.default)

    def apply(config: Config): Builder = new Builder(config)

    def apply(root: File): Builder = new Builder(Config.default, Some(root))

    def setup(root: File): Builder = apply().setup(root)

    def setup(config: Config, root: File): Builder = new Builder(config).setup(root)

    // def setup(root: File): Builder = {
    //   val builder = apply(root)
    //   val files = root.listFiles.toVector
    //   val cursor = builder.cursor
    //   for (a <- files)
    //     _setup(cursor, a)
    //   builder
    // }

    // private def _setup(cursor: TreeCursor[Realm.Data], p: File): Unit =
    //   if (p.isDirectory)
    //     _setup_directory(cursor, p)
    //   else
    //     _setup_file(cursor, p)

    // private def _setup_directory(cursor: TreeCursor[Realm.Data], p: File): Unit = {
    //   cursor.enter(p.getName)
    //   val files = p.listFiles.toVector
    //   for (a <- files)
    //     _setup(cursor, a)
    //   cursor.leave()
    // }

    // private def _setup_file(cursor: TreeCursor[Realm.Data], p: File): Unit = {
    //   val name = p.getName.toLowerCase
    //   val suffix = StringUtils.toSuffix(name)
    //   if (_is_text(suffix))
    //     cursor.set(name, _to_text(p))
    //   else if (_is_binary(suffix))
    //     cursor.set(name, _to_binary(p))
    // }

    // private def _is_text(suffix: String) = suffix match {
    //   case "dox" => true
    //   case "md" => true
    //   case "markdown" => true
    //   case "org" => true
    //   case "html" => true
    //   case _ => false
    // }

    // private def _is_binary(suffix: String) = false // TODO

    // private def _to_text(p: File) = StringData(IoUtils.toText(p))

    // private def _to_binary(p: File) = FileData(p)
  }

  trait Visitor extends TreeVisitor[Realm.Data] {
  }

  case class NodeView(node: TreeNode[Realm.Data]) {
    def name = node.name
    def getNameSuffix: Option[String] = node.getNameSuffix
    def getContent: Option[Realm.Data] = Option(node.content)
  }
  object NodeView {
    val empty = NodeView(TreeNode.empty())
  }

  def create() = Realm(new PlainTree())

  def create(directory: File): Realm = Builder.setup(directory).build()
  def create(directory: Path): Realm = create(directory.toFile)
  def create(directory: String): Realm = create(new File(directory))

  def create(
    config: Builder.Config,
    directory: File
  ): Realm = Builder.setup(config, directory).build()
  def create(
    config: Builder.Config,
    directory: Path
  ): Realm = create(config, directory.toFile)
  def create(
    config: Builder.Config,
    directory: String
  ): Realm = create(config, new File(directory))
}
