package org.goldenport.matrix

import org.apache.commons.math3.stat.regression.SimpleRegression

/*
 * @since   Feb. 26, 2020
 * @version Feb. 26, 2020
 * @author  ASAMI, Tomoharu
 */
case class GoldenportNumericalOperations() extends INumericalOperations {
  def simpleRegression(p: IMatrix[Double]): (Double, Double) = {
    val li = 0
    val ri = 1
    val sr = new SimpleRegression(true)
    for (y <- 0 until p.height) {
      sr.addData(p.at(li, y), p.at(ri, y))
    }
    val c = sr.getIntercept()
    val s = sr.getSlope()
    (c, s)
  }
}

object GoldenportNumericalOperations extends INumericalOperations {
  val default = GoldenportNumericalOperations()

  def simpleRegression(p: IMatrix[Double]): (Double, Double) = default.simpleRegression(p)
}
