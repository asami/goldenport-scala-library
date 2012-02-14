package org.goldenport.util

/**
 * @since   Jul. 29, 2010
 * @version May. 23, 2011
 * @author  ASAMI, Tomoharu
 */
object Control {
  def or[A, B](exprs: (A => Option[B])*)(param: A): Option[B] = {
    for (expr <- exprs) {
      val r = expr(param)
      if (r.isDefined) return r
    }
    None
  }

  def orElse[A, B](exprs: (A => Option[B])*)(defaultvalue: B)(param: A): B = {
    for (expr <- exprs) {
      val r = expr(param)
      if (r.isDefined) return r.get
    }
    defaultvalue
  }

  /**
   * orElse with default Function
   */
  def orElseF[A, B](exprs: (A => Option[B])*)(defaultfunc: A => B)(param: A): B = {
    for (expr <- exprs) {
      val r = expr(param)
      if (r.isDefined) return r.get
    }
    defaultfunc(param)
  }

  /**
   * orElse with default ByName Function
   */
  def orElseN[A, B](exprs: (A => Option[B])*)(defaultfunc: => B)(param: A): B = {
    for (expr <- exprs) {
      val r = expr(param)
      if (r.isDefined) return r.get
    }
    defaultfunc
  }

  /**
   * orElse with default ByName Function with two parameters
   */
  def orElseN[A1, A2, B](exprs: ((A1, A2) => Option[B])*)(defaultfunc: => B)(param1: A1, param2: A2): B = {
    for (expr <- exprs) {
      val r = expr(param1, param2)
      if (r.isDefined) return r.get
    }
    defaultfunc
  }

  /**
   * orElse PartialFunctions
   */
  def orElseP[A, B](exprs: PartialFunction[A, B]*)(defaultvalue: B)(param: A): B = {
    for (expr <- exprs) {
      if (expr.isDefinedAt(param)) return expr(param)
    }
    defaultvalue
  }

  /**
   * orElse PartialFunctions
   */
  def orElsePF[A, B](exprs: PartialFunction[A, B]*)(defaultfunc: A => B)(param: A): B = {
    for (expr <- exprs) {
      if (expr.isDefinedAt(param)) return expr(param)
    }
    defaultfunc(param)
  }

  /**
   * orElse PartialFunctions
   */
  def orElsePN[A, B](exprs: PartialFunction[A, B]*)(defaultfunc: => B)(param: A): B = {
    for (expr <- exprs) {
      if (expr.isDefinedAt(param)) return expr(param)
    }
    defaultfunc
  }

  // Collection operation

  // find and map
  def or[A, B](params: Traversable[A])(func: A => Option[B]): Option[B] = {
    for (p <- params) {
      val r = func(p)
      if (r.isDefined) return r
    }
    None
  }

  // XXX collect?
  def orP[A, B](params: Traversable[A])(func: PartialFunction[A, B]): Option[B] = {
    for (p <- params) {
      if (func.isDefinedAt(p)) return Some(func(p))
    }
    None
  }
}

case class Or[T, U](funcs: PartialFunction[T, U]*) extends PartialFunction[T, U] {
  def isDefinedAt(in: T) = funcs.exists(_.isDefinedAt(in))

  def apply(in: T) = {
    funcs.find(_.isDefinedAt(in)) match {
      case Some(pf) => pf.apply(in)
      case None => throw new MatchError(in)
    }
  }
}
