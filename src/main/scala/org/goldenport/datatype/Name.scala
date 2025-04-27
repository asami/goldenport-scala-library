package org.goldenport.datatype

import org.goldenport.context.Consequence
import org.goldenport.util.StringUtils

/*
 * @since   Apr. 19, 2025
 * @version Apr. 27, 2025
 * @author  ASAMI, Tomoharu
 */
abstract class Name() extends Datatype {
  import Name._

  def name: String

  protected def name_Min: Int = NAME_MIN
  protected def name_Max: Int = NAME_MAX
  protected def is_Valid(p: String): Boolean = isNameString(p)

  require (name.length >= name_Min, s"Too short: ${name.length}")
  require (name.length <= name_Max, s"Too large: ${name.length}")
  require (is_Valid(name), s"Invalid name")

  override def toString() = name
}

object Name {
  val NAME_MIN = 1
  val NAME_MAX = 64

  def apply(name: String): Name = SimpleName(name)

  def parse(name: String): Consequence[Name] = ???

  def isNameChar(c: Char): Boolean =
    !(StringUtils.isAsciiChar(c) && !StringUtils.isSafeUriChar(c))

  def isNameString(s: String): Boolean =
    s.length > 0 && s.forall(isNameChar)
}

case class SimpleName(name: String) extends Name {
}
