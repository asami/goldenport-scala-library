package org.goldenport.typeclass

/*
 * @since   Nov.  2, 2025
 * @version Nov.  2, 2025
 * @author  ASAMI, Tomoharu
 */
trait Zero[A] {
  def zero: A
}

object Zero {
  def apply[A](implicit z: Zero[A]): Zero[A] = z

  def instance[A](a: A): Zero[A] = new Zero[A] {
    def zero: A = a
  }

  implicit val stringZero: Zero[String] = instance("")
  implicit def listZero[A]: Zero[List[A]] = instance(Nil)
  implicit def optionZero[A]: Zero[Option[A]] = instance(None)
  implicit val intZero: Zero[Int] = instance(0)
  implicit val doubleZero: Zero[Double] = instance(0.0)
}
