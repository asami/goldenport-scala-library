package org.goldenport.cli.spec

import org.goldenport.value._

/*
 * @since   Mar.  2, 2025
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait Multiplicity extends NamedValueInstance {
}
object Multiplicity {
  case object One extends Multiplicity {
    val name = "one"
  }
  case object ZeroOne extends Multiplicity {
    val name = "zeroone"
  }
  case object OneMore extends Multiplicity {
    val name = "onemore"
  }
  case object ZeroMore extends Multiplicity {
    val name = "zeromore"
  }
}
