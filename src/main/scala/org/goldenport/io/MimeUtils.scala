package org.goldenport.io

import scalaz._
import org.goldenport.Strings.mimetype._
import org.goldenport.util.StringUtils

/*
 * Deprecated. Use MimeType instead.
 *
 * @since   Oct. 22, 2015
 *  version Aug. 29, 2017
 *  version Jun. 24, 2019
 * @version Dec.  8, 2019
 * @author  ASAMI, Tomoharu
 */
object MimeUtils {
  type MimeType = String
  type Suffix = String

  // See MimeType
  def filenameToMimetype(filename: String): Option[\/[Suffix, MimeType]] = {
    StringUtils.getSuffix(filename).map(x => x.toLowerCase match {
      case "json" => \/-(application_json)
      case "csv" => \/-(text_csv)
      case "tsv" => \/-(text_tsv)
      case "xsv" => \/-(text_xsv)
      case "lcsv" => \/-(text_lcsv)
      case "ltsv" => \/-(text_ltsv)
      case "lxsv" => \/-(text_lxsv)
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
