package org.goldenport

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 * @version Sep.  2, 2017
 * @author  ASAMI, Tomoharu
 */
package object exception {
  object RAISE {
    def noReachDefect: Nothing = throw new NoReachDefectException()
    def notImplementedYetDefect: Nothing = throw new NotImplementedYetDefectException()
    def unsupportedOperationFault: Nothing = throw new UnsupportedOperationException()
    def missingPropertyFault(name: String): Nothing = throw new MissingPropertyFaultException(name)
    def invalidArgumentFault(s: String): Nothing = throw new IllegalArgumentException(s)
  }
}
