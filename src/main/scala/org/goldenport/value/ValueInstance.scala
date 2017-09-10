package org.goldenport.value

/*
 * @since   Jan. 22, 2017
 *  version Feb. 10, 2017
 *  version Jul. 12, 2017
 * @version Aug. 29, 2017
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

trait ValueClass[T <: ValueInstance] {
  def isCaseSensible = true
  protected final def normalize_key(s: String) = s.toLowerCase
  def get(s: String): Option[T]
  def apply(s: String): T = get(normalize_key(s)) getOrElse {
    throw new IllegalArgumentException("Invalid value")
  }
}

trait EnumerationClass[T <: ValueInstance] extends ValueClass[T] {
  def elements: Seq[T]
  def get(s: String): Option[T] = elements.find(_.toString == s)
}
