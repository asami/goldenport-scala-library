package org.goldenport.extension

import org.goldenport.RAISE

/*
 * @since   Jan.  1, 2019
 * @version Aug.  2, 2023
 * @author  ASAMI, Tomoharu
 */
trait IDocument extends Showable {
//  def factory: IDocumentFactory
  def display = print
  def show = print
}

object IDocument {
  val empty = EmptyDocument
}

case object EmptyDocument extends IDocument {
//  def factory: IDocumentFactory = RAISE.unsupportedOperationFault
  def print = ""
}

case class Text(s: String) extends IDocument {
  def print = s
}

case class Description(
  name: Option[String],
  title: IDocument,
  summary: IDocument,
  content: IDocument
) extends IDocument {
  def print = content.print
}
object Description {
  val empty = Description(None, EmptyDocument, EmptyDocument, EmptyDocument)
}

// trait IDocumentFactory {
//   def createDescription(
//     name: Option[String],
//     title: IDocument,
//     summary: IDocument,
//     content: IDocument
//   ): IDocument
// }
