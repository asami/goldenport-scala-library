package org.goldenport.io

import java.io.File
import java.net.URL
import java.nio.charset.Charset
import org.goldenport.value._
import org.goldenport.bag.Bag

/*
 * @since   Mar. 13, 2025
 * @version Mar. 14, 2025
 * @author  ASAMI, Tomoharu
 */
class Exporter(config: Exporter.Config) {
  import Exporter._

  def export(out: OutputSink, in: InputSource) {
    config.exportStrategy match {
      case ExportStrategy.Overwrite => new Overwriter(config).export(out, in)
      case ExportStrategy.Update => new Updater(config).export(out, in)
    }
  }

  def export(file: File, string: String) {
    export(OutputSink(file), InputSource(string, config.charset))
  }

  def export(file: File, url: URL) {
    export(OutputSink(file), InputSource(url))
  }

  def export(file: File, in: File) {
    export(OutputSink(file), InputSource(in))
  }

  def export(file: File, bag: Bag) {
    export(OutputSink(file), InputSource(bag))
  }
}

object Exporter {
  case class Config(
    charset: Charset,
    exportStrategy: ExportStrategy = ExportStrategy.Overwrite
  ) {
  }

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

  class Overwriter(config: Config) {
    def export(sink: OutputSink, source: InputSource) = sink match {
      case FileOutputSink(file) => IoUtils.save(file, source)
    }
  }

  class Updater(config: Config) {
    def export(sink: OutputSink, source: InputSource) = {
      if (!IoUtils.isSame(sink, source))
        IoUtils.save(sink, source)
    }
  }
}
