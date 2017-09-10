package org.goldenport.bag

import java.net.URL
import com.asamioffice.goldenport.io.UURL

/*
 * @since   Oct. 23, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
class UrlBag(val url: URL) extends ChunkBag with ReadOnlyFeature
    with FilenameMimeFeature {
  protected def uri_Name: String = url.toString
  def openInputStream() = url.openStream()
}

object UrlBag {
  def fromUri(uri: String) = {
    new UrlBag(UURL.getURLFromFileOrURLName(uri))
  }

  def fromUrl(url: URL) = {
    new UrlBag(url)
  }
}
