package org.goldenport.xsv

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._

/*
 * @since   Oct. 11, 2019
 *  version Oct. 27, 2019
 * @version Feb. 29, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LxsvSpec extends WordSpec with Matchers with GivenWhenThen {
  def create(p: String): Lxsv = Lxsv.create(p)
  def lxsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.XsvStrategy, p +: ps)
  def lcsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.CsvStrategy, p +: ps)
  def ltsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.TsvStrategy, p +: ps)
  def lsvcsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.SCsvStrategy, p +: ps)
  def lssv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.SsvStrategy, p +: ps)

  "Lxsv" should {
    "Lsxv" which {
      "one" in {
        create("a:1") should be(lxsv('a -> 1))
      }
    }
    "delimiter" which {
      "comma" in {
        create("a:1,b:2") should be(lcsv('a -> 1, 'b -> 2))
      }
      "tab" in {
        create("a:1\tb:2") should be(ltsv('a -> 1, 'b -> 2))
      }
      "tab and array" in {
        create("a:1\tb:2,3,4") should be(ltsv('a -> "1", 'b -> "2,3,4"))
      }
      "semi-colon" in {
        create("a:1;b:2") should be(lsvcsv('a -> 1, 'b -> 2))
      }
      "space" in {
        create("a:1 b:2") should be(lssv('a -> 1, 'b -> 2))
      }
    }
    "no label" which {
      "one" in {
        create("1") should be(lxsv('_1 -> 1))
      }
      "two" in {
        create("1,2") should be(lcsv('_1 -> 1, '_2 -> 2))
      }
    }
    "double quote" which {
      "one" in {
        create("""a:"A"""") should be(lxsv('a -> "A"))
      }
      "tab and array" in {
        create("a:\"1,2,3\"\tb:\"2,3,4\"") should be(ltsv('a -> "1,2,3", 'b -> "2,3,4"))
      }
      "control char" in {
        create("""a:"A\nB"""") should be(lxsv('a -> "A\nB"))
      }
    }
  }
}
