package org.goldenport.scalaz

import scalaz.std.AllInstances
import scalaz.syntax.std._
import scalaz.syntax.ToBindOps  // for >>= and flatMap
import scalaz.syntax.ToFunctorOps  // for map
import scalaz.syntax.ToApplicativeOps  // for |@|
import scalaz.syntax.ToTraverseOps
import org.goldenport.scalaz.syntax.FoldTraverseSyntax._

/*
 * @since   Oct.  1, 2025
 * @version Oct.  1, 2025
 * @author  ASAMI, Tomoharu
 */
object implicits
  extends AllInstances
     with ToBindOps
     with ToFunctorOps
     with ToApplicativeOps
     with ToTraverseOps
