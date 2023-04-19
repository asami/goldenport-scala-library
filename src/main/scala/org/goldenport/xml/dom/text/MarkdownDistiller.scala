package org.goldenport.xml.dom.text

import javax.xml.xpath._
import javax.xml.transform._
import org.w3c.dom._
import org.goldenport.xml.dom._

/*
 * @since   Apr. 18, 2023
 * @version Apr. 19, 2023
 * @author  ASAMI, Tomoharu
 */
class MarkdownDistiller(
  val rule: MarkdownDistiller.Rule,
  val document: Document
) extends DomTransformerHelper {
  def apply(node: Node): String = {
    val a = rule.xpaths.toStream.flatMap(_apply_xpath(_, node)).headOption.getOrElse(node)
    val b = rule.xslt.flatMap(_apply_xslt(_, node)).headOption.getOrElse(a)
    val r = b
    MarkdownMaker.make(r)
  }

  private def _apply_xpath(expr: XPathExpression, p: Node): Option[Node] = {
    val a = transform_xpath(expr, p)
    if (is_empty_text(a))
      None
    else
      Some(a)
  }

  private def _apply_xslt(xslt: Transformer, p: Node): Option[Node] =
    Some(transform_xslt(xslt, p))
}

object MarkdownDistiller {
  private lazy val _xpath = XPathFactory.newInstance().newXPath()

  case class Rule(
    xpaths: List[XPathExpression] = Nil, // or
    xslt: Option[Transformer] = None
  ) {
  }

  def makeXPaths(xpaths: Seq[String], p: String): String = {
    val rule = Rule(xpaths.map(_xpath_expression).toList)
    val doc = DomUtils.parseHtmlLowerCase(p)
    val maker = new MarkdownDistiller(rule, doc)
    maker.apply(doc)
  }

  private def _xpath_expression(p: String) = {
    _xpath.compile(p)
  }
}
