package org.goldenport.io

import scalaz._
import org.goldenport.Strings.mimetype._
import org.goldenport.util.StringUtils

/*
 * @since   Oct. 22, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
object MimeUtils {
  type MimeType = String
  type Suffix = String

  def filenameToMimetype(filename: String): Option[\/[Suffix, MimeType]] = {
    StringUtils.getSuffix(filename).map(x => x.toLowerCase match {
      case "json" => \/-(application_json)
      case "csv" => \/-(text_csv)
      case "xls" => \/-(application_excel)
      case "xlsx" => \/-(application_excel)
      case "zip" => \/-(application_zip)
      case _ => -\/(x)
    })
  }

  def getMimetype(filename: String): Option[MimeType] = {
    filenameToMimetype(filename).flatMap(_.toOption)
  }
}
