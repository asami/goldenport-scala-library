package org.goldenport

/*
 * @snice   Aug. 30, 2017
 *  version Aug. 30, 2017
 *  version Sep.  2, 2017
 *  version Oct. 29, 2017
 *  version Jan. 12, 2018
 *  version Aug. 28, 2018
 * @version Dec. 31, 2018
 * @author  ASAMI, Tomoharu
 */
package object exception {
  object RAISE {
    def noReachDefect: Nothing = throw new NoReachDefectException()
    def noReachDefect(msg: String): Nothing = throw new NoReachDefectException(msg)
    def noReachDefect(o: Object, msg: String): Nothing = noReachDefect(o.getClass, msg: String)
    def noReachDefect(c: Class[_], msg: String): Nothing = throw new NoReachDefectException(s"${c.getSimpleName}#${msg}")
    def notImplementedYetDefect: Nothing = throw new NotImplementedYetDefectException()
    def notImplementedYetDefect(msg: String): Nothing = throw new NotImplementedYetDefectException(msg)
    def notImplementedYetDefect(o: Object, msg: String): Nothing = notImplementedYetDefect(o.getClass, msg)
    def notImplementedYetDefect(c: Class[_], msg: String): Nothing = throw new NotImplementedYetDefectException(s"${c.getSimpleName}#${msg}")
    def illegalConfigurationDefect(s: String): Nothing = throw new IllegalConfigurationDefectException(s)
    def unsupportedOperationFault: Nothing = throw new UnsupportedOperationException()
    def unsupportedOperationFault(msg: String): Nothing = throw new UnsupportedOperationException(msg)
    def missingPropertyFault(name: String): Nothing = throw new MissingPropertyFaultException(name)
    def invalidArgumentFault(s: String): Nothing = throw new IllegalArgumentException(s)
    def noSuchElement(name: String): Nothing = throw new NoSuchElementException(name)
    def syntaxErrorFault(s: String): Nothing = throw new SyntaxErrorFaultException(s)
  }
}
