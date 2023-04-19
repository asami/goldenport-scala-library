package org.goldenport.xml.dom.text

import org.w3c.dom._

/*
 * @since   Apr. 18, 2023
 * @version Apr. 18, 2023
 * @author  ASAMI, Tomoharu
 */
class MarkdownMaker(
) extends HtmlTextMaker {
  def apply(p: Node) = {
    make_text(p)
    toString()
  }

  protected def make_Ul(p: Element): Unit = {
    make_Block(p) // TODO
  }

  protected def make_Ol(p: Element): Unit = {
    make_Block(p) // TODO
  }

  protected def make_Dl(p: Element): Unit = {
    make_Block(p) // TODO
  }

  protected def make_Table(p: Element): Unit = {
    make_Block(p) // TODO
  }
}

object MarkdownMaker {
  def apply(): MarkdownMaker = new MarkdownMaker()

  def make(p: Node): String = apply().apply(p)
}
