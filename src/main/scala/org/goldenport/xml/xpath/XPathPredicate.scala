package org.goldenport.xml.xpath

/*
 * @since   Mar.  6, 2021
 * @version Mar.  6, 2021
 * @author  ASAMI, Tomoharu
 */
case class XPathPredicate(
  expressions: List[XPathPredicate.Expression]
) {
  def andExpression: String =
    if (expressions.isEmpty)
      ""
    else
      expressions.map(_.expression).mkString(" and ", " and ", "")

  def orExpression: String = expressions match {
    case Nil => ""
    case x :: Nil => s" or ${x.expression}"
    case xs => s""" or (${expressions.map(_.expression).mkString(" and ")})"""
  }
}

object XPathPredicate {
  sealed trait Expression {
    def expression: String

    protected def element_name(ignorecasep: Boolean, name: String) =
      if (ignorecasep)
        s"""translate(local-name(), 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')='${name.toUpperCase}'"""
      else
        s"""local-name()='$name'"""
  }

  case class ChildElementContainText(
    elementName: String,
    text: String,
    ignorecasep: Boolean = true
  ) extends Expression {
    def expression: String = {
      s"""child::*[${element_name(ignorecasep, elementName)} and contains(text(), '$text')]"""
    }
  }

  def apply(p: Expression, ps: Expression*): XPathPredicate = XPathPredicate((p +: ps).toList)
}
