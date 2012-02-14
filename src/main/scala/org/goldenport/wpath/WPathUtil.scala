package org.goldenport.wpath

import com.asamioffice.goldenport.text.UPathString.concatPathname

/**
 * @since   Sep. 28, 2010
 * @version Sep. 28, 2010
 * @author  ASAMI, Tomoharu
 */
object WPathUtil {
  def concat(parent: String, child: String): String = {
    if (child.indexOf(':') != -1) child
    else if (parent == ".") child
    else concatPathname(parent, child)
  }
}
