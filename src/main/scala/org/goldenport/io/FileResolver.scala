package org.goldenport.io

import scala.util.Try
import java.io.File
import org.goldenport.bag.ChunkBag

/*
 * @since   Jul. 17, 2025
 * @version Jul. 19, 2025
 * @author  ASAMI, Tomoharu
 */
class FileResolver(context: FileResolver.Context) {
  def resolve(path: String): Option[ChunkBag] = {
    val candidats = context.makeCandidates(path)
    candidats.toStream.flatMap(x => Try(x.asBag).toOption).headOption
  }
}

object FileResolver {
  case class Context(
    resources: Vector[ResourceLocator],
    parameters: FileTextResolver.Parameters = FileTextResolver.Parameters.empty
  ) {
    def withParameters(params: FileTextResolver.Parameters) = copy(parameters = params)

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
  }
  object Context {
    val default = create(new File("."))

    def create(p: File, ps: File*): Context = Context(
      (p +: ps).toVector.map(FileResourceLocator)
    )
  }
}
