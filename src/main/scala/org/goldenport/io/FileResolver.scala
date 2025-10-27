package org.goldenport.io

import scala.util.Try
import java.net.URI
import java.net.URL
import java.io.File
import org.goldenport.context.Consequence
import org.goldenport.context.Conclusion
import org.goldenport.bag.ChunkBag

/*
 * @since   Jul. 17, 2025
 *  version Jul. 19, 2025
 *  version Aug. 10, 2025
 *  version Sep. 12, 2025
 * @version Oct. 24, 2025
 * @author  ASAMI, Tomoharu
 */
class FileResolver(context: FileResolver.Context) {
  def directoryForWrite = context.directoryForWrite

  // def resolve(path: String): Consequence[ChunkBag] = {
  //   val candidats = context.makeCandidates(path)
  //   candidats match {
  //     case Seq() => Consequence.notFound(path)
  //     case x +: Seq() => Consequence(x.asBag)
  //     case x +: xx +: xs =>
  //       val a = Consequence(x.asBag)
  //       a match {
  //         case m: Consequence.Success[_] => m
  //         case _ => _resolve(xx, xs) orElse a
  //       }
  //   }
  // }

  def resolve(path: String): Consequence[ChunkBag] =
    resolveWithLocator(path).map(_._2)

  def resolveWithLocator(path: String): Consequence[(ResourceLocator, ChunkBag)] = {
    val candidats = context.makeCandidates(path)
    candidats match {
      case Seq() => Consequence.notFound(path)
      case x +: Seq() => _locator_bag(x)
      case x +: xx +: xs =>
        val a = _locator_bag(x)
        a match {
          case m: Consequence.Success[_] => m
          case _ => _resolve(xx, xs) orElse a
        }
    }
  }

  private def _locator_bag(p: InputSource): Consequence[(ResourceLocator, ChunkBag)] = Consequence {
    val l = ResourceLocator.get(p) getOrElse Conclusion.noReachDefect(s"No location: $p").RAISE
    (l, p.asBag)
  }

  def toFile(uri: URI): Consequence[File] = directoryForWrite match {
    case Some(s) => UriUtils.getFile(s, uri) match {
      case Some(ss) => Consequence.success(ss)
      case None => Consequence.badRequest(s"No suitable for file: $uri")
    }
    case None => Consequence.notFound("No directory to write.")
  }

  def toFile(url: URL): Consequence[File] =
    UrlUtils.getFile(url) match {
      case Some(ss) => Consequence.success(ss)
      case None => Consequence.badRequest(s"No suitable for file: $url")
    }

  @annotation.tailrec
  private def _resolve(p: InputSource, ps: Vector[InputSource]): Consequence[(ResourceLocator, ChunkBag)] =
    _locator_bag(p) match {
      case m: Consequence.Success[_] => m
      case m => ps.headOption match {
        case Some(s) => _resolve(s, ps.tail)
        case None => m
      }
    }

  def resolveString(path: String): Consequence[String] =
    for {
      x <- resolve(path)
      s <- Consequence(x.toText(context.encoding))
    } yield s
}

object FileResolver {
  case class Context(
    resources: Vector[ResourceLocator],
    parameters: FileTextResolver.Parameters = FileTextResolver.Parameters.empty
  ) {
    def withParameters(params: FileTextResolver.Parameters) = copy(parameters = params)

    def withBaseFile(p: URI) = {
      val container = UriUtils.toContainer(p)
      if (resources.exists(_.isSame(container)))
        this
      else
        copy(resources = ResourceLocator.make(container) +: resources)
    }

    def encoding: String = parameters.encoding getOrElse "UTF-8"

    def makeCandidates(path: String): Vector[InputSource] =
      resources.flatMap { x =>
        x.getFile match {
          case Some(f) => Some(FileInputSource(new File(f, path)))
          case None => x.getUrl match {
            case Some(u) => Some(UrlInputSource(UrlUtils.addPath(u, path)))
            case None => None
          }
        }
      }

    def directoryForWrite: Option[File] = resources.flatMap(_.getFile).headOption
  }
  object Context {
    val default = create(new File("."))

    def create(p: File, ps: File*): Context = Context(
      (p +: ps).toVector.map(FileResourceLocator)
    )
  }
}
