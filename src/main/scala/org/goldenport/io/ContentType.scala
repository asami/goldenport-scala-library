package org.goldenport.io

import java.nio.charset.Charset
import org.goldenport.Strings

/*
 * @since   Sep. 18, 2018
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
case class ContentType(
  mime: MimeType,
  charset: Option[Charset]
) {
}

object ContentType {
  val UTF8 = Charset.forName("UTF-8")
  val octetstream = ContentType(MimeType.application_octet_stream, None)
  val html = ContentType(MimeType.text_html, Some(UTF8))

  def parse(p: String): ContentType = {
    Strings.totokens(p, ";") match {
      case Nil => octetstream
      case x :: Nil => ContentType(MimeType.as(x), None)
      case x :: y :: _ =>
        val c = Strings.totokens(y, "=") match {
          case Nil => None
          case _ :: Nil => None
          case _ :: z :: _ => Some(Charset.forName(z))
        }
        ContentType(MimeType.as(x), c)
    }
  }
}
