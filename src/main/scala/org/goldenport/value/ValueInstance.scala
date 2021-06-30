package org.goldenport.value

import org.goldenport.util.StringUtils
import org.goldenport.parser.ParseResult

/*
 * @since   Jan. 22, 2017
 *  version Feb. 10, 2017
 *  version Jul. 12, 2017
 *  version Aug. 29, 2017
 *  version Oct. 14, 2017
 *  version Aug. 17, 2018
 *  version Sep. 24, 2018
 *  version Mar. 21, 2021
 * @version Jun. 14, 2021
 * @author  ASAMI, Tomoharu
 */
trait ValueInstance {
  protected def value_string: String

  override def toString() = value_string
}
trait NamedValueInstance extends ValueInstance {
  def name: String
  def value_string = name
}

trait ClassNamedValueInstance extends NamedValueInstance {
  def name: String = StringUtils.classNameToHypenName(name_Suffix, this)
  protected def name_Suffix: String
}

trait ValueClass[T <: ValueInstance] {
  def isCaseSensible = true
  protected final def normalize_key(s: String) = s.toLowerCase
  def get(s: String): Option[T] // TODO
  def getIgnoreCase(s: String): Option[T]
  def apply(s: String): T = get(normalize_key(s)) getOrElse {
    throw new IllegalArgumentException(s"Invalid value: $s")
  }
  def takeIgnoreCase(s: String): T = getIgnoreCase(normalize_key(s)) getOrElse {
    throw new IllegalArgumentException(s"Invalid value: $s")
  }
  def parse(s: String): ParseResult[T] = get(normalize_key(s)).
    map(ParseResult.success).getOrElse(ParseResult.error(s"Invalid value: $s"))
}

trait EnumerationClass[T <: ValueInstance] extends ValueClass[T] {
  def elements: Seq[T]
  def get(s: String): Option[T] = elements.find(_.toString == s)
  def getIgnoreCase(s: String): Option[T] = elements.find(_.toString.equalsIgnoreCase(s))
}
