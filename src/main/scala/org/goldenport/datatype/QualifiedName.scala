package org.goldenport.datatype

/*
 * @since   Aug. 17, 2025
 * @version Aug. 17, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class QualifiedName() extends Name() {
  import QualifiedName._

  override protected def name_Min: Int = PATHNAME_MIN
  override protected def name_Max: Int = PATHNAME_MAX
  override protected def is_Valid(p: String): Boolean = isQualifiedNameString(p)
}

object QualifiedName {
  val PATHNAME_MIN = 1
  val PATHNAME_MAX = 128

  case class Plain(name: String) extends QualifiedName {
  }

  def apply(name: String): QualifiedName = Plain(name)

  def isQualifiedNameChar(c: Char): Boolean = Name.isNameChar(c) || c == '.'

  def isQualifiedNameString(s: String): Boolean =
    s.length > 0 && s.forall(isQualifiedNameChar)
}
