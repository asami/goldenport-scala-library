package org.goldenport.xsv

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.xml._

/*
 * @since   Oct. 11, 2019
 * @version Oct. 12, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LxsvSpec extends WordSpec with Matchers with GivenWhenThen {
  def create(p: String): Lxsv = Lxsv.create(p)
  def xsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.XsvStrategy, p +: ps)
  def csv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.CsvStrategy, p +: ps)
  def tsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.TsvStrategy, p +: ps)
  def scsv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.SCsvStrategy, p +: ps)
  def ssv(p: (Symbol, Any), ps: (Symbol, Any)*): Lxsv = Lxsv(Xsv.SsvStrategy, p +: ps)

  "Lxsv" should {
    "Lsxv" which {
      "one" in {
        create("a:1") should be(xsv('a -> 1))
      }
    }
    "delimiter" which {
      "comma" in {
        create("a:1,b:2") should be(csv('a -> 1, 'b -> 2))
      }
      "tab" in {
        create("a:1\tb:2") should be(tsv('a -> 1, 'b -> 2))
      }
      "tab and array" in {
        create("a:1\tb:2,3,4") should be(tsv('a -> "1", 'b -> "2,3,4"))
      }
      "semi-colon" in {
        create("a:1;b:2") should be(scsv('a -> 1, 'b -> 2))
      }
      "space" in {
        create("a:1 b:2") should be(ssv('a -> 1, 'b -> 2))
      }
    }
    "no label" which {
      "one" in {
        create("1") should be(xsv('_0 -> 1))
      }
      "two" in {
        create("1,2") should be(csv('_0 -> 1, '_1 -> 2))
      }
    }
    "double quote" which {
      "one" in {
        create("""a:"A"""") should be(xsv('a -> "A"))
      }
      "tab and array" in {
        create("a:\"1,2,3\"\tb:\"2,3,4\"") should be(tsv('a -> "1,2,3", 'b -> "2,3,4"))
      }
    }
  }
}
