package org.goldenport.datatype

import org.goldenport.context.Consequence
import org.goldenport.util.StringUtils

/*
 * @since   Apr. 19, 2025
 *  version Apr. 27, 2025
 *  version May. 16, 2025
 *  version Jun. 24, 2025
 *  version Nov. 18, 2025
 * @version Apr.  1, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class Name() extends Datatype {
  import Name._

  protected def print_String = name

  def name: String

  def length: Int = name.codePointCount(0, name.length)

  def size: Int = name.length

  protected def name_Min: Int = NAME_MIN
  protected def name_Max: Int = NAME_MAX
  protected def is_Valid(p: String): Boolean = isNameString(p)

  require (length >= name_Min, s"Too short: ${name.length}")
  require (length <= name_Max, s"Too large: ${name.length}")
  require (is_Valid(name), s"Invalid name")

  def toTitle: String = StringUtils.makeTitle(name)

  override def toString() = name
}

object Name {
  val NAME_MIN = 1
  val NAME_MAX = 256

  def apply(name: String): Name = SimpleName(name)

  def parse(name: String): Consequence[Name] = Consequence(apply(name))

  def isNameChar(c: Char): Boolean =
    !(StringUtils.isAsciiChar(c) && !StringUtils.isSafeUriChar(c))

  def isNameString(s: String): Boolean =
    s.length > 0 && s.forall(isNameChar)
}

case class SimpleName(name: String) extends Name {
}
