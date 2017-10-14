package org.goldenport.io

import java.net.{URL, URI}
import scalax.io._

/*
 * @since   Oct.  9, 2017
 * @version Oct.  9, 2017
 * @author  ASAMI, Tomoharu
 */
object IoUtils {
  def toText(url: URL): String = Resource.fromURL(url).string
}

