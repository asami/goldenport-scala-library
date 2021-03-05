package org.goldenport.util

import java.net.URI
import java.io.File
import org.goldenport.cli.Environment

/*
 * @since   Jan. 11, 2021
 * @version Jan. 11, 2021
 * @author  ASAMI, Tomoharu
 */
object UriUtils {
  def showTerse(workdir: File, uri: URI): String =
    uri.getScheme match {
      case "file" =>
        val a = workdir.toURI relativize uri
        printPathQueryFragment(a)
      case _ => uri.toString
    }

  def showTerse(env: Environment, uri: URI): String = 
    uri.getScheme match {
      case "file" =>
        getRelative(env.workDirectory, uri).
          map(x => printPathQueryFragment(x)).getOrElse(
            getRelative(env.homeDirectory, uri).
              map(x => s"~/${printPathQueryFragment(x)}").getOrElse(
                env.getProjectDirectory.
                  flatMap(dir => getRelative(dir, uri)).
                  map(x => s"@/${printPathQueryFragment(x)}").
                  getOrElse(printPathQueryFragment(uri))
              )
          )
      case _ => uri.toString
    }

  def getRelative(base: File, uri: URI): Option[URI] = getRelative(base.toURI, uri)

  def getRelative(base: URI, uri: URI): Option[URI] = {
    val a = base relativize uri
    if (a == uri)
      None
    else
      Some(a)
  }

  def printPathQueryFragment(p: URI): String = {
    val path = Option(p.getPath)
    val query = Option(p.getQuery)
    val fragment = Option(p.getFragment)
    path.fold("")(identity) + query.fold("")(x => s"?$x") + fragment.fold("")(x => s"#$x")
  }
}
