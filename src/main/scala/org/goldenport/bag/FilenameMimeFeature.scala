package org.goldenport.bag

import org.goldenport.Strings
import org.goldenport.io.MimeType
import org.goldenport.util.StringUtils

/*
 * @since   Oct. 23, 2015
 *  version Aug. 29, 2017
 * @version May. 19, 2020
 * @author  ASAMI, Tomoharu
 */
trait FilenameMimeFeature { self: Bag =>
  protected def uri_Name: String
  override def name = StringUtils.pathLastComponentBody(uri_Name)
  override def filenameSuffix = StringUtils.getSuffix(uri_Name)
  override def mimetype: MimeType = {
    filenameSuffix.flatMap(MimeType.getBySuffix) getOrElse MimeType.application_octet_stream
  }
}
