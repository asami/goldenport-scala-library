package org.goldenport.cli

import java.net.URL
import org.goldenport.cli.spec.{Parameter => SpecParameter}

/*
 * @since   Oct.  5, 2018
 *  version Oct.  8, 2018
 * @version Apr. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class Property(
  name: String,
  value: Argument
) {
  def asUrlList: List[URL] = value.asUrlList
}

object Property {
}
