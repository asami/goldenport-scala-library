package org.goldenport.util

/*
 * @since   Jun. 26, 2019
 * @version Jun. 26, 2019
 * @author  ASAMI, Tomoharu
 */
object StringBuilderUtils {
  def concat[T](ps: Seq[CharSequence]): StringBuilder = {
    val sb = new StringBuilder()
    ps.foreach(sb.append)
    sb
  }

  def concat[T](sb: StringBuilder, ps: Seq[CharSequence]): StringBuilder = {
    ps.foreach(sb.append)
    sb
  }

  def concat[T](sb: StringBuilder, p: CharSequence, ps: CharSequence*): StringBuilder = {
    sb.append(p)
    ps.foreach(sb.append)
    sb
  }

  def concat[T](
    f: (StringBuilder, T) => Unit
  )(
    ps: Seq[T]
  ): StringBuilder = {
    val sb = new StringBuilder()
    ps.headOption.map { x0 =>
      f(sb, x0)
      for (x <- ps.tail) {
        f(sb, x)
      }
    }
    sb
  }

  def concatInfixDelimiter[T](
    f: (StringBuilder, T) => Unit,
    infix: CharSequence
  )(ps: Seq[T]): StringBuilder = {
    val sb = new StringBuilder()
    ps.headOption.map { x0 =>
      f(sb, x0)
      for (x <- ps.tail) {
        sb.append(infix)
        f(sb, x)
      }
    }
    sb
  }
}
