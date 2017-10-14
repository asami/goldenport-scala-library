package org.goldenport.io

import java.net.URI

/**
 * @since   Sep. 24, 2012
 * @version Oct.  6, 2017
 * @author  ASAMI, Tomoharu
 */
case class UriAux(
  query: Map[String, List[String]] = Map.empty,
  fragment: Option[String] = None
) {
}

object UriAux {
  def apply(uri: URI): UriAux = {
    val q: Map[String, List[String]] = {
      uri.getQuery match {
        case null => Map.empty
        case "" => Map.empty
        case query => {
          val a = if (query.startsWith("?")) query.substring(1) else query
          val b = {
            val i = a.lastIndexOf("#")
            if (i == -1) a
            else a.substring(0, i)
          }
          val c = b.split("&")
          val d = c.map(_.split("=").toList) collect {
            case x :: y :: xs => x -> List(y)
            case x :: Nil => x -> Nil
          }
          Map(d: _*)
        }
      }
    }
    val f = Option(uri.getFragment)
    UriAux(q, f)
  }
}
