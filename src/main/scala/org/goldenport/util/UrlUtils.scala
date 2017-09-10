package org.goldenport.util

import scala.util.control.NonFatal
import java.net.URL
import com.asamioffice.goldenport.io.UURL

/*
 * @since   Jul. 24, 2017
 * @version Aug. 30, 2017
 * @author  ASAMI, Tomoharu
 */
object UrlUtils {
  def takeLeafName(url: URL): String =
    StringUtils.pathLastComponent(url.getPath)

  def isExist(url: URL): Boolean =
    Option(UURL.getActiveFile(url)).fold {
      try {
        for (in <- resource.managed(url.openStream())) {
          in.read() // try read one byte
        }
        true
      } catch {
        case NonFatal(e) => false
      }
    } { file =>
      file.exists
    }
}
