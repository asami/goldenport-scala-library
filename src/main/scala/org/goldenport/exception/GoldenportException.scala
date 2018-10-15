package org.goldenport.exception

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 *  version Oct. 29, 2017
 * @version Jan. 12, 2018
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
  message: String
) extends GoldenportException(message)
