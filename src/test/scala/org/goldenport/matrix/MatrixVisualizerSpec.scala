package org.goldenport.matrix

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._

/*
 * @since   Jun. 22, 2019
 * @version Jun. 22, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class MatrixVisualizerSpec extends WordSpec with Matchers with GivenWhenThen {
  "a" should {
    "b" which {
      "c" in {
        val matrix = VectorRowColumnMatrix(Vector(
          Vector(1, 2, 3, 4),
          Vector(10, 20, 30, 40)
        ))
        val mv = MatrixVisualizer.border((x: Int) => x.toString)
        val r = mv.plainText(matrix)
        println(r)
      }
    }
  }
}
