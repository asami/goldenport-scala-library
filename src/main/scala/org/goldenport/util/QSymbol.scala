package org.goldenport.util

import scala.collection.immutable

/**
 * @since   Aug. 29, 2010
 * @version Jan.  9, 2011
 * @author  ASAMI, Tomoharu
 */
class QSymbol(val atoms: List[Symbol]) extends immutable.Seq[Symbol] {
  def name = atoms.map(_.name).mkString(".")
  def path = atoms.map(_.name).mkString("/")

  // Seq
  def apply(idx: Int) = atoms.apply(idx)
  def length = atoms.length
  def iterator = atoms.iterator

  def container = new QSymbol(atoms.dropRight(1))
  def leaf = atoms.last

  // Object
  override def equals(o: Any) = {
    o match {
      case qs: QSymbol => qs.atoms == atoms
      case _ => false
    }
  }

  override def hashCode() = {
    (0 /: atoms)((x: Int, y: Symbol) => x + y.hashCode)
  }
}

object QSymbol {
  def apply(atoms: Symbol*) = new QSymbol(atoms.toList)

  def apply(atom: Symbol) = new QSymbol(List(atom))

  def apply(string: String) = {
    if (string.startsWith("/")) {
      new QSymbol(string.substring(1).split("/").map(Symbol(_)).toList)
    } else {
      new QSymbol(string.split("/").map(Symbol(_)).toList)
    }
  }
}

class QSymbolSet(val qsymbols: QSymbol*) extends immutable.Set[QSymbol] {
  def path = qsymbols.length match {
    case 0 => ""
    case 1 => qsymbols(0).name
    case _ => "*/" + qsymbols.map(_.name).mkString("/")
  }

  def names = qsymbols.map(_.name).toList

  // Set
  def -(elem: QSymbol) = new QSymbolSet((Set(qsymbols: _*) - elem).toList: _*)

  def +(elem: QSymbol) = new QSymbolSet((Set(qsymbols: _*) + elem).toList: _*)

  def contains(elem: QSymbol) = qsymbols.contains(elem)
  def iterator = qsymbols.iterator
}

object QSymbolSet {
  def apply(atom: Symbol) = new QSymbolSet(QSymbol(atom))

  def apply(atom: QSymbol) = new QSymbolSet(atom)

  def apply(atom: String) = new QSymbolSet(QSymbol(Symbol(atom)))

  def apply(atoms: Seq[QSymbol]) = new QSymbolSet(atoms: _*)
}
