package org.goldenport.datatype

/*
 * @since   Jul.  1, 2025
 * @version Jul.  4, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class PathName() extends Name() {
  import PathName._

  override protected def name_Min: Int = PATHNAME_MIN
  override protected def name_Max: Int = PATHNAME_MAX
  override protected def is_Valid(p: String): Boolean = isPathNameString(p)
}

object PathName {
  val PATHNAME_MIN = 1
  val PATHNAME_MAX = 128

  def isPathNameChar(c: Char): Boolean = Name.isNameChar(c) || c == '/'

  def isPathNameString(s: String): Boolean =
    s.length > 0 && s.forall(isPathNameChar)
}
