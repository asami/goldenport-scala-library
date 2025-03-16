package org.goldenport.i18n

import scalaz._, Scalaz._
import org.goldenport.util.StringUtils

/*
 * @since   Jan.  2, 2021
 *  version Feb. 25, 2022
 *  version Mar. 19, 2022
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class StringFormatter() {
  def isWordSeparating(prev: Char, next: Char): Boolean = StringFormatter.isWordSeparating(prev, next)

  def stringConsoleWidth(p: String): Int = StringFormatter.display.stringWidth(p)

  def charConsoleWidth(p: Char): Int = StringFormatter.display.charWidth(p)
}

object StringFormatter {
  val default = StringFormatter()
  val symbolCR = "■"
  val symbolLF = "□"
  val symbolCRLF = symbolCR + symbolLF

  def isWordSeparating(prev: Char, next: Char): Boolean =
    StringUtils.isAsciiChar(prev) && StringUtils.isAsciiChar(next)

  object display {
    def stringWidth(p: String): Int = StringUtils.stringConsoleWidth(p)

    def charWidth(p: Char): Int = StringUtils.charConsoleWidth(p)

    def substring(p: String, size: Int): String = {
      @annotation.tailrec
      def _go_(cs: List[Char], physicalwidth: Int, rs: Vector[Char]): Seq[Char] = cs match {
        case Nil => rs
        case x :: xs =>
          val a = physicalwidth - StringUtils.charConsoleWidth(x)
          if (a == 0)
            rs :+ x
          else if (a < 0)
            rs
          else
            _go_(xs, a, rs :+ x)
      }
      _go_(p.toList, size, Vector.empty).mkString
    }

    def cutstring(p: String, size: Int): String = {
      @annotation.tailrec
      def _go_(cs: List[Char], physicalwidth: Int, rs: Vector[Char]): Seq[Char] = cs match {
        case Nil => rs
        case x :: xs =>
          val a = physicalwidth - StringUtils.charConsoleWidth(x)
          if (a == 0)
            rs :+ x
          else if (a < 0)
            _pad(rs, physicalwidth)
          else
            _go_(xs, a, rs :+ x)
      }
      _go_(p.toList, size, Vector.empty).mkString
    }

    private def _pad(ps: Seq[Char], size: Int): Seq[Char] =
      (ps ++ List.fill(size)('.'))

    def shrink(p: String, width: Int, tail: Int): String = {
      case class Slot(c: Char, width: Int)

      def _slots_(s: String): List[Slot] = s.toList.map(x => Slot(x, charWidth(x)))

      def _make_(width: Int, ps: Seq[Slot]): String = {
        def _go_(w: Int, pxs: List[Slot], rs: Vector[Char]): Vector[Char] = {
          // println(s"_make_._go($w, $pxs, $rs)")
          val r = pxs match {
            case Nil => rs
            case x :: Nil => x.width match {
              case 1 => w match {
                case 0 => rs
                case _ => rs :+ x.c
              }
              case 2 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case _ => rs :+ x.c
              }
            }
            case x :: x1 :: Nil => x.width match {
              case 1 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => _go_(w - 1, List(x1), rs :+ x.c)
                case n => _go_(w - 1, List(x1), rs :+ x.c)
              }
              case 2 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => rs ++ ".."
                case n => _go_(w - 2, List(x1), rs :+ x.c)
              }
            }
            case x :: x1 :: x2 :: Nil if x1.width == 1 && x2.width == 1 => x.width match {
              case 1 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => rs ++ ".."
                case 3 => rs :+ x1.c :+ x2.c
                case n => _go_(w - 1, List(x1, x2), rs :+ x.c)
              }
              case 2 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => rs ++ ".."
                case n => _go_(w - 2, List(x1), rs :+ x.c)
              }
            }
            case x :: xs => x.width match {
              case 1 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => rs ++ ".."
                case n => _go_(w - 1, xs, rs :+ x.c)
              }
              case 2 => w match {
                case 0 => rs
                case 1 => rs :+ '.'
                case 2 => rs ++ ".."
                case n => _go_(w - 2, xs, rs :+ x.c)
              }
            }
          }
          // println(s"_make._go_($width, $ps, $rs) => $r")
          r
        }
        _go_(width, ps.toList, Vector.empty).mkString
      }

      @annotation.tailrec
      def _go_(cs: List[Char], physicalwidth: Int, rs: List[Char]): Seq[Char] = cs match {
        case Nil => rs
        case x :: xs =>
          val a = physicalwidth - StringUtils.charConsoleWidth(x)
          if (a == 0)
            x :: rs
          else if (a < 0)
            rs
          else
            _go_(xs, a, x :: rs)
      }
      val right: Seq[Char] = _go_(p.reverse.toList, tail, Nil)
      val remaindersize: Int = width - right.length
      val candidatestring = p.substring(0, p.length - right.length)
      val candateslots = _slots_(candidatestring)
      val candidatewidth: Int = candateslots.foldMap(_.width)
      val left = if (remaindersize - candidatewidth >= 0)
        candidatestring
      else
        _make_(remaindersize, candateslots)
      // println(s"right: $right")
      // println(s"remaindersize: $remaindersize")
      // println(s"slots: $candateslots")
      // println(s"candidatestring: $candidatestring")
      // println(s"candidatewidth: $candidatewidth")
      // println(s"left: $left")
      left ++ right
    }

    def enLeft(p: String, width: Int): String = {
      val n = stringWidth(p)
      if (n >= width)
        p
      else
        p ++ Vector.fill(width - n)(' ')
    }

    def enCenter(p: String, width: Int): String = {
      val n = stringWidth(p)
      if (n >= width) {
        p
      } else {
        val w = width - n
        val x = w / 2
        if (w % 2 == 0)
          (Vector.fill(x)(' ') ++ p ++ Vector.fill(x)(' ')).mkString
        else
          (Vector.fill(x)(' ') ++ p ++ Vector.fill(x + 1)(' ')).mkString
      }
    }

    def enRight(p: String, width: Int): String = {
      val n = stringWidth(p)
      if (n >= width)
        p
      else
        (Vector.fill(width - n)(' ') ++ p).mkString
    }

    def embed(p: String, width: Int): String = {
      val a = escapeDisplay(p)
      val tail = (width / 2) - 2
      shrink(a, width, tail)
    }

    def escapeDisplay(s: String): String =
      if (s.contains('\n') | s.contains('\r'))
        s.replace("\r\n", symbolCRLF).replace("\n", symbolLF).replace("\r", symbolCR)
      else
        s

    def escapeDisplayOld(s: String): String =
      if (s.contains('\n') | s.contains('\r'))
        s.replace("\r\n", "♼").replace("\n", "♲").replace("\r", "♻")
      else
        s
  }
}
