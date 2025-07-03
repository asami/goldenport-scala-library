package org.goldenport.datatype

/*
 * @since   Jul.  1, 2025
 * @version Jul.  1, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class PathName() extends Name() {
  import PathName._

  override protected def is_Valid(p: String): Boolean = isPathNameString(p)
}

object PathName {
  def isPathNameChar(c: Char): Boolean = Name.isNameChar(c) || c == '/'

  def isPathNameString(s: String): Boolean =
    s.length > 0 && s.forall(isPathNameChar)
}
