package org.goldenport.extension

import org.goldenport.RAISE

/*
 * @since   Jan.  1, 2019
 * @version Jan.  1, 2019
 * @author  ASAMI, Tomoharu
 */
trait IDocument {
//  def factory: IDocumentFactory
}

object IDocument {
  val empty = EmptyDocument
}

case object EmptyDocument extends IDocument {
//  def factory: IDocumentFactory = RAISE.unsupportedOperationFault
}

case class Text(s: String) extends IDocument {
}

case class Description(
  name: Option[String],
  title: IDocument,
  summary: IDocument,
  content: IDocument
) extends IDocument {
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
