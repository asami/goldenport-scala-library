package org.goldenport.xml.dom.text

import org.w3c.dom._
import org.goldenport.xml.dom._

/*
 * @since   Apr. 18, 2023
 * @version Apr. 18, 2023
 * @author  ASAMI, Tomoharu
 */
abstract class HtmlTextMaker(
) extends TextMaker {
  protected def make_Element(p: Element) = {
    val name = DomUtils.localNameLowerCase(p)
    name match {
      case "em" => make_Em(p)
      case "strong" => make_Strong(p)
      case "b" => make_B(p)
      case "i" => make_I(p)
      case "u" => make_U(p)
      case "small" => make_Small(p)
      case "cite" => make_Cite(p)
      case "code" => make_Code(p)
      case "kbd" => make_Kbd(p)
      case "var" => make_Var(p)
      case "samp" => make_Samp(p)
      case "abbr" => make_Abbr(p)
      case "q" => make_Q(p)
      case "time" => make_Time(p)
      case "mark" => make_Mark(p)
      case "a" => make_A(p)
      case "img" => make_Img(p)
      case "span" => make_Span(p)
      case "article" => make_Article(p)
      case "section" => make_Section(p)
      case "main" => make_Main(p)
      case "p" => make_P(p)
      case "div" => make_Div(p)
      case "blockquote" => make_Blockquote(p)
      case "pre" => make_Pre(p)
      case "ul" => make_Ul(p)
      case "ol" => make_Ol(p)
      case "dl" => make_Dl(p)
      case "table" => make_Table(p)
      case "br" => make_Br(p)
      case _ => make_Block(p)
    }
  }

  protected def make_Em(p: Element): Unit = make_Span(p)

  protected def make_Strong(p: Element): Unit = make_Span(p)

  protected def make_B(p: Element): Unit = make_Span(p)

  protected def make_I(p: Element): Unit = make_Span(p)

  protected def make_U(p: Element): Unit = make_Span(p)

  protected def make_Small(p: Element): Unit = make_Span(p)

  protected def make_Cite(p: Element): Unit = make_Span(p)

  protected def make_Code(p: Element): Unit = make_Span(p)

  protected def make_Kbd(p: Element): Unit = make_Span(p)

  protected def make_Var(p: Element): Unit = make_Span(p)

  protected def make_Samp(p: Element): Unit = make_Span(p)

  protected def make_Abbr(p: Element): Unit = make_Span(p)

  protected def make_Q(p: Element): Unit = make_Span(p)

  protected def make_Time(p: Element): Unit = make_Span(p)

  protected def make_Mark(p: Element): Unit = make_Span(p)

  protected def make_A(p: Element): Unit = make_Span(p)

  protected def make_Img(p: Element): Unit = make_Span(p)

  protected def make_Span(p: Element): Unit = make_Inline(p)

  protected def make_Inline(p: Element): Unit = {
    make_children(p)
  }

  protected def make_Article(p: Element): Unit = make_Block(p)

  protected def make_Section(p: Element): Unit = make_Block(p)

  protected def make_Main(p: Element): Unit = make_Block(p)

  protected def make_Blockquote(p: Element): Unit = make_Block(p)

  protected def make_Pre(p: Element): Unit = make_Block(p)

  protected def make_P(p: Element): Unit = make_Div(p)

  protected def make_Div(p: Element): Unit = make_Block(p)

  protected def make_Ul(p: Element): Unit

  protected def make_Ol(p: Element): Unit

  protected def make_Dl(p: Element): Unit

  protected def make_Table(p: Element): Unit

  protected def make_Br(p: Element): Unit = {
    println()
  }

  protected def make_Block(p: Element): Unit = {
    make_children(p)
    println()
  }

  protected final def make_children(p: Node): Unit = {
    for (x <- DomUtils.children(p))
      make_text(x)
  }
}

object HtmlTextMaker {
}
