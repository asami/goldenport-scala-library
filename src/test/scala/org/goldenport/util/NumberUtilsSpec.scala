package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import java.math.MathContext
import java.math.RoundingMode
import java.math.RoundingMode._
import org.goldenport.collection.VectorMap

/*
 * @since   Jan. 27, 2022
 * @version Jan. 27, 2022
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class NumberUtilsSpec extends WordSpec with Matchers with GivenWhenThen {
  import NumberUtils._

  val MC_UP = new MathContext(2, UP)
  val MC_DOWN = new MathContext(2, DOWN)
  val MC_CEILING = new MathContext(2, CEILING)
  val MC_FLOOR = new MathContext(2, FLOOR)
  val MC_HALF_UP = new MathContext(2, HALF_UP)
  val MC_HALF_DOWN = new MathContext(2, HALF_DOWN)
  val MC_HALF_EVEN = new MathContext(2, HALF_EVEN)
  val MC_UNNECESSARY = new MathContext(2, UNNECESSARY)

  val NaN = Int.MinValue

  private def _decimal(p: Double) = BigDecimal(p.toString)

  private def _do(rm: RoundingMode): Unit = {
    for ((x, r) <- data(rm)) {
      if (r == NaN) {
        intercept[ArithmeticException] {
          roundScaleZero(x, rm)
        }
      } else {
        // println(s"$rm, ($x => $r) => ${roundScaleZero(x, rm)}")
        roundScaleZero(x, rm) should be(r)
      }
    }
  }

  val ddata = VectorMap(
    UP -> List(5.5 -> 6, 2.5 -> 3, 1.6 -> 2, 1.1 -> 2, 1.0 -> 1, -1.0 -> -1, -1.1 -> -2, -1.6 -> -2, -2.5 -> -3, -5.5 -> -6, 25.5 -> 26),
    DOWN -> List(5.5 -> 5, 2.5 -> 2, 1.6 -> 1, 1.1 -> 1, 1.0 -> 1, -1.0 -> -1, -1.1 -> -1, -1.6 -> -1, -2.5 -> -2, -5.5 -> -5),
    CEILING -> List(5.5 -> 6, 2.5 -> 3, 1.6 -> 2, 1.1 -> 2, 1.0 -> 1, -1.0 -> -1, -1.1 -> -1, -1.6 -> -1, -2.5 -> -2, -5.5 -> -5),
    FLOOR -> List(5.5 -> 5, 2.5 -> 2, 1.6 -> 1, 1.1 -> 1, 1.0 -> 1, -1.0 -> -1, -1.1 -> -2, -1.6 -> -2, -2.5 -> -3, -5.5 -> -6),
    HALF_UP -> List(5.5 -> 6, 2.5 -> 3, 1.6 -> 2, 1.1 -> 1, 1.0 -> 1, -1.0 -> -1, -1.1 -> -1, -1.6 -> -2, -2.5 -> -3, -5.5 -> -6),
    HALF_DOWN -> List(5.5 -> 5, 2.5 -> 2, 1.6 -> 2, 1.1 -> 1, 1.0 -> 1, -1.0 -> -1, -1.1 -> -1, -1.6 -> -2, -2.5 -> -2, -5.5 -> -5),
    HALF_EVEN -> List(5.5 -> 6, 2.5 -> 2, 1.6 -> 2, 1.1 -> 1, 1.0 -> 1, -1.0 -> -1, -1.1 -> -1, -1.6 -> -2, -2.5 -> -2, -5.5 -> -6),
    UNNECESSARY -> List(5.5 -> NaN, 2.5 -> NaN, 1.6 -> NaN, 1.1 -> NaN, 1.0 -> 1, -1.0 -> -1, -1.1 -> NaN, -1.6 -> NaN, -2.5 -> NaN, -5.5 -> NaN)
  )
  val data = ddata.map {
    case (rm, xs) => rm -> xs.map(x => _decimal(x._1) -> x._2)
  }

  "roundScaleZero" should {
    "UP" which {
      "all" in {
        _do(UP)
      }
    }
    "DOWN" which {
      "all" in {
        _do(DOWN)
      }
    }
    "CEILING" which {
      "all" in {
        _do(CEILING)
      }
    }
    "FLOOR" which {
      "all" in {
        _do(FLOOR)
      }
    }
    "HALF_UP" which {
      "all" in {
        _do(HALF_UP)
      }
    }
    "HALF_DOWN" which {
      "all" in {
        _do(HALF_DOWN)
      }
    }
    "HALF_EVEN" which {
      "all" in {
        _do(HALF_EVEN)
      }
    }
    "UNNECESSARY" which {
      "all" in {
        _do(UNNECESSARY)
      }
    }
  }
}
