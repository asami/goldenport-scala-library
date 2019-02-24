package org.goldenport.exception

import org.goldenport.parser.ParseLocation

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 *  version Oct. 29, 2017
 *  version Jan. 12, 2018
 * @version Feb.  2, 2019
 * @author  ASAMI, Tomoharu
 */
abstract class GoldenportException(
  message: String,
  cause: Throwable = null
) extends RuntimeException(message, cause) {
}
object GoldenportException {
  def message(m: String, d: String) = if (m != null) m else d
}

class NoReachDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "No reach"))

class NotImplementedYetDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "Not implemented yet"))

class IllegalConfigurationDefectException(
  m: String = null
) extends GoldenportException(GoldenportException.message(m, "Illegal configuration"))

class MissingPropertyFaultException(
  name: String
) extends GoldenportException(name)

class SyntaxErrorFaultException(
  message: String,
  cause: Throwable = null,
  location: Option[ParseLocation] = None
) extends GoldenportException(message)
object SyntaxErrorFaultException {
  def apply(message: String, location: ParseLocation): SyntaxErrorFaultException =
    new SyntaxErrorFaultException(message, null, Some(location))
}
