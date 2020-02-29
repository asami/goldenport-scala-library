package org.goldenport.matrix

/*
 * @since   Feb. 26, 2020
 * @version Feb. 26, 2020
 * @author  ASAMI, Tomoharu
 */
trait INumericalOperations {
  def simpleRegression(p: IMatrix[Double]): (Double, Double)
}
