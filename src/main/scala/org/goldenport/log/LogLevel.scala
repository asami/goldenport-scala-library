package org.goldenport.log

import org.goldenport.value._

/*
 * @since   Oct.  6, 2018
 * @version Oct.  6, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait LogLevel extends NamedValueInstance {
}

object LogLevel extends EnumerationClass[LogLevel] {
  val elements = Vector(Error, Warn, Info, Debug, Trace)

  case object Error extends LogLevel {
    val name = "error"
  }

  case object Warn extends LogLevel {
    val name = "warn"
  }

  case object Info extends LogLevel {
    val name = "info"
  }

  case object Debug extends LogLevel {
    val name = "debug"
  }

  case object Trace extends LogLevel {
    val name = "trace"
  }
}
