package org.goldenport.scalaz.syntax

import scala.language.higherKinds
import scalaz._
import org.goldenport.scalaz.FoldTraverseUtil

/*
 * @since   Oct.  1, 2025
 * @version Oct.  1, 2025
 * @author  ASAMI, Tomoharu
 */
object FoldTraverseSyntax {
  implicit class FoldTraverseOps[A](val self: Seq[A]) extends AnyVal {
    def intercalateTraverse[F[_]: Monad, B](sep: F[B])(f: A => F[B]): F[Vector[B]] =
      FoldTraverseUtil.intercalateTraverse(self, sep)(f)

    def intercalateTraverse_[F[_]: Monad](sep: F[Unit])(f: A => F[Unit]): F[Unit] =
      FoldTraverseUtil.intercalateTraverse_(self, sep)(f)

    def intercalateFoldLeft[B: Monoid](sep: B)(implicit ev: A <:< B): B =
      FoldTraverseUtil.intercalateFoldLeft(self.map(ev), sep)

    def intercalateFoldMap[B: Monoid](sep: B)(f: A => B): B =
      FoldTraverseUtil.intercalateFoldMap(self, sep)(f)

    def intercalateFoldMap_[B: Monoid](sep: B)(f: A => B): Unit =
      FoldTraverseUtil.intercalateFoldMap_(self, sep)(f)
  }
} 
