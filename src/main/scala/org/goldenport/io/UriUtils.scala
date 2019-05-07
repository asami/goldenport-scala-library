package org.goldenport.io

import scala.util.control.NonFatal
import java.net.URI
import com.asamioffice.goldenport.io.UURL

/*
 * @since   Oct.  6, 2017
 * @version Apr. 26, 2019
 * @author  ASAMI, Tomoharu
 */
object UriUtils {
  def addPath(uri: URI, path: String) = UriBuilder(uri).addPath(path).build
  def sibling(uri: URI): URI = UriBuilder.byPath("..").addPath(uri).build
}
