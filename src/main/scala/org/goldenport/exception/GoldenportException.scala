package org.goldenport.exception

/*
 * @snice   Aug. 30, 2017
 * @version Aug. 30, 2017
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
) extends GoldenportException(GoldenportException.message(m, "Not Implemented yet"))

class MissingPropertyFaultException(
  name: String
) extends GoldenportException(name)
