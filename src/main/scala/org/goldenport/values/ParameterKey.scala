package org.goldenport.values

import org.goldenport.exception.RAISE
import org.goldenport.Strings

/*
 * @since   Jul. 30, 2019
 * @version Jul. 30, 2019
 * @author  ASAMI, Tomoharu
 */
case class ParameterKey(
  origin: String,
  key: String,
  path: String,
  pathList: List[String],
  adornment: Option[String],
  adornmentArguments: List[String]
) {
}

object ParameterKey {
  val PATH_DELIMITER = "."
  val ADORNMENT_DELIMITER = "__"
  val ADORNMENT_IN_DELIMITER = "_"

  def parse(p: Symbol): ParameterKey = parse(p.name)

  def parse(p: String): ParameterKey = {
    val i = p.indexOf(ADORNMENT_DELIMITER)
    if (i == -1) {
      val path = p
      val pathlist: List[String] = Strings.totokens(path, PATH_DELIMITER)
      val key = pathlist.head
      ParameterKey(p, key, path, pathlist, None, Nil)
    } else {
      val path = p.substring(0, i)
      val pathlist: List[String] = Strings.totokens(path, PATH_DELIMITER)
      val key = pathlist.head
      val ad = p.substring(path.length + ADORNMENT_DELIMITER.length)
      val ads = Strings.totokens(ad, ADORNMENT_IN_DELIMITER)
      ads.headOption.map { x => 
        ParameterKey(p, key, path, pathlist, Some(x), ads.tail)
      }.getOrElse(
        ParameterKey(p, key, path, pathlist, None, Nil)
      )
    }
  }
}
