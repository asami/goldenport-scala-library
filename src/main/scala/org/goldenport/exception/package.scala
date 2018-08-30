package org.goldenport

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 *  version Sep.  2, 2017
 *  version Oct. 29, 2017
 *  version Jan. 12, 2018
 * @version Aug. 28, 2018
 * @author  ASAMI, Tomoharu
 */
package object exception {
  object RAISE {
    def noReachDefect: Nothing = throw new NoReachDefectException()
    def noReachDefect(msg: String): Nothing = throw new NoReachDefectException(msg)
    def notImplementedYetDefect: Nothing = throw new NotImplementedYetDefectException()
    def notImplementedYetDefect(msg: String): Nothing = throw new NotImplementedYetDefectException(msg)
    def illegalConfigurationDefect(s: String): Nothing = throw new IllegalConfigurationDefectException(s)
    def unsupportedOperationFault: Nothing = throw new UnsupportedOperationException()
    def unsupportedOperationFault(msg: String): Nothing = throw new UnsupportedOperationException(msg)
    def missingPropertyFault(name: String): Nothing = throw new MissingPropertyFaultException(name)
    def invalidArgumentFault(s: String): Nothing = throw new IllegalArgumentException(s)
    def syntaxErrorFault(s: String): Nothing = throw new SyntaxErrorFaultException(s)
  }
}
