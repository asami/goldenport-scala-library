package org.goldenport.bag

import org.goldenport.Strings
import org.goldenport.io.MimeUtils
import org.goldenport.util.StringUtils

/*
 * @since   Oct. 23, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
trait FilenameMimeFeature { self: Bag =>
  protected def uri_Name: String
  override def name = StringUtils.pathLastComponentBody(uri_Name)
  override def filenameSuffix = StringUtils.getSuffix(uri_Name)
  override def mimetype = {
    MimeUtils.getMimetype(uri_Name) getOrElse Strings.mimetype.application_octet_stream
  }
}
