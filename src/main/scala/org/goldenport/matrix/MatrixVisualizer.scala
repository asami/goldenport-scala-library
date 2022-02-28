package org.goldenport.matrix

import scalaz.{Align => _, _}, Scalaz._
import org.goldenport.RAISE
import org.goldenport.value._
import org.goldenport.util.StringUtils
import org.goldenport.util.AnyUtils
import MatrixVisualizer._

/*
 * @since   Jun. 16, 2019
 *  version Jul.  7, 2019
 *  version Aug. 24, 2019
 *  version Oct. 11, 2019
 *  version Nov. 30, 2019
 *  version Mar.  8, 2021
 * @version Feb. 24, 2022
 * @author  ASAMI, Tomoharu
 */
case class MatrixVisualizer[T](
  topBorder: BorderStyle,
  bottomBorder: BorderStyle,
  leftBorder: Boolean,
  rightBorder: Boolean,
  horizontalSeparator: Boolean,
  lineStyle: LineStyle,
  newline: String,
  textf: (ColumnDef, T) => String,
  columnDefs: ColumnDefs = ColumnDefs.empty,
  horizontalSeparatorGap: Option[Int] = Some(5),
  isCompact: Boolean = false
) {
  private def _is_horizontal_separator(i: Int): Boolean =
    horizontalSeparator && _is_gap(i)

  private def _is_gap(i: Int): Boolean = horizontalSeparatorGap.
    map(i + 1 % _ == 0).
    getOrElse(false)

  def withLineStyle(p: LineStyle) = copy(lineStyle = p)

  def withColumnDefs(p: ColumnDefs) = copy(columnDefs = p)

  def withCompact(p: Boolean) = copy(isCompact = p)

  def buildColumns(p: IMatrix[T]): ColumnInfos = {
    val raw = _build_raw(p)
    _build_columns(raw)
  }

  def buildColumnsCompact(width: Int, p: IMatrix[T]): ColumnInfos = {
    val r = buildColumns(p)
    if (r.width <= width)
      r
    else
      _compact(width, r)
  }

  def plainText(p: IMatrix[T]): String = {
    val raw = _build_raw(p)
    val columns = _build_columns(raw)
    val r = _make(columns, raw)
    _draw(columns, r)
  }

  def plainText(columns: ColumnInfos, p: IMatrix[T]): String = {
    val raw = _build_raw(p)
    val r = _make(columns, raw)
    _draw(columns, r)
  }

  def plainTextCenter(columns: ColumnInfos, p: IMatrix[T]): String = {
    val raw = _build_raw(p)
    val r = _make_center(columns, raw)
    _draw(columns, r)
  }

  private def _build_raw(p: IMatrix[T]): Rows = {
    val rs = for (y <- 0 until p.height) yield {
      val cs = for (x <- 0 until p.width) yield {
        val c = columnDefs(x)
        val v = p(x, y)
        val t = textf(c, v)
        Cell(t)
      }
      Row(cs)
    }
    Rows(rs)
  }

  private def _build_columns(p: Rows): ColumnInfos = {
    case class Z(columns: Vector[Int] = Vector.fill(p.width)(0)) {
      def r = ColumnInfos(columns.map(ColumnInfo(_)))

      def +(rhs: Row) = {
        val a = columns.zip(rhs.cells.map(_string_width)).zipWithIndex
        val b = a map {
          case ((l, r), i) => columnDefs.get(i).
              flatMap(_.width).
              getOrElse(l max r)
        }
        Z(b)
      }

      private def _string_width(p: Cell): Int = p.lines.toVector.foldMap(stringWidth)
    }
    p.rows./:(Z())(_+_).r
  }

  private def _compact(width: Int, p: ColumnInfos) = {
    val avgwidth = ((width - 2) / p.length) - 1

    sealed trait Slot {
      def column: ColumnInfo
      def isUnresolved: Boolean
    }
    case class Resolved(column: ColumnInfo, width: Int) extends Slot {
      val isUnresolved = false
    }
    case class Unresolved(column: ColumnInfo) extends Slot {
      val isUnresolved = true
    }

    case class Z(xs: Vector[Slot] = Vector.empty, remainder: Int = width - 1 - (p.length + 1)) {
      def r = if (remainder <= 0)
        _asis
      else
        _compact

      private def _asis = xs.map {
        case Resolved(c, w) => c.withWidth(w)
        case Unresolved(c) => c
      }

      private def _compact = {
        val undefinedcount = xs.count(_.isUnresolved)
        val basewidth = {
          val a = remainder / undefinedcount
          if (remainder - ((a + 1) * undefinedcount) >= 0)
            a
          else
            a - 1
        }
        val fraction = width - ((basewidth + 1) * undefinedcount)
        case class ZZ(r: Vector[ColumnInfo] = Vector.empty, fraction: Int = fraction) {
          def +(rhs: Slot) = rhs match {
            case Resolved(c, w) => copy(r = r :+ c.withWidth(w))
            case Unresolved(c) =>
              if (fraction > 0) {
                val w = basewidth + 1
                copy(r = r :+ c.withWidth(w), fraction = fraction - 1)
              } else {
                copy(r = r :+ c.withWidth(basewidth))
              }

          }
        }
        xs./:(ZZ())(_+_).r
      }

      def +(rhs: ColumnInfo) =
        if (rhs.width <= avgwidth)
          resolved(rhs, rhs.width)
        else
          unresolved(rhs)

      def unresolved(p: ColumnInfo) = copy(xs = xs :+ Unresolved(p))

      def resolved(p: ColumnInfo, width: Int) = copy(
        xs = xs :+ Resolved(p, width),
        remainder = remainder - (p.width + 1)
      )
    }
    val r = p.columns./:(Z())(_+_).r
    ColumnInfos(r)
  }

  private def _make(columns: ColumnInfos, rows: Rows): Rows =
    _make(columns, Align.Left, rows)

  private def _make_center(columns: ColumnInfos, rows: Rows): Rows =
    _make(columns, Align.Center, rows)

  private def _make(columns: ColumnInfos, align: Align, rows: Rows): Rows = {
    val a: Seq[Row] = rows.rows.map { row =>
      val xs = row.effectiveCells(columns).zip(columns.columns).map {
        case (cell, column) => cell.normalize(column, align) // cell.normalize(column, charWidth)
      }
      Row(xs.toVector)
    }
    Rows(a.toVector)
  }

  private def _draw(columns: ColumnInfos, rows: Rows): String = {
    val sb = new StringBuilder()
    _draw_border(sb, columns, topBorder)
    rows.headOption.map { first =>
      _draw_row(sb, columns, first)
      for ((row, i) <- rows.tailWithIndex) {
        if (_is_horizontal_separator(i))
          _draw_horizontal_separator(sb, columns)
        _draw_row(sb, columns, row)
      }
    }
    _draw_border(sb, columns, bottomBorder)
    sb.toString
  }

  private def _draw_border(sb: StringBuilder, columns: ColumnInfos, style: BorderStyle) {
    style match {
      case NoneBorder => // do nothing
      case TopEndBorder => _draw_top(sb, columns)
      case BottomEndBorder => _draw_bottom(sb, columns)
      case MiddleBorder => _draw_middle(sb, columns)
    }
  }

  private def _draw_top(sb: StringBuilder, columns: ColumnInfos) {
    sb.append(lineStyle.topLeftCorner)
    columns.columns.headOption.map { first =>
      _fill(sb, first, lineStyle.topBorder)
      columns.columns.tail.map { x =>
        sb.append(lineStyle.topSeparator)
        _fill(sb, x, lineStyle.topBorder)
      }
    }
    sb.append(lineStyle.topRightCorner)
    sb.append(newline)
  }

  private def _draw_bottom(sb: StringBuilder, columns: ColumnInfos) {
    sb.append(lineStyle.bottomLeftCorner)
    columns.columns.headOption.map { first =>
      _fill(sb, first, lineStyle.bottomBorder)
      columns.columns.tail.map { x =>
        sb.append(lineStyle.bottomSeparator)
        _fill(sb, x, lineStyle.bottomBorder)
      }
    }
    sb.append(lineStyle.bottomRightCorner)
    sb.append(newline)
  }

  private def _draw_middle(sb: StringBuilder, columns: ColumnInfos) {
    sb.append(lineStyle.middleLeft)
    columns.columns.headOption.map { first =>
      _fill(sb, first, lineStyle.middleSeparator)
      columns.columns.tail.map { x =>
        sb.append(lineStyle.middleVerticalSeparator)
        _fill(sb, x, lineStyle.middleSeparator)
      }
    }
    sb.append(lineStyle.middleRight)
    sb.append(newline)
  }

  private def _draw_horizontal_separator(sb: StringBuilder, columns: ColumnInfos) {
    sb.append(lineStyle.leftHorizontalSeparator)
    columns.columns.headOption.map { first =>
      _fill(sb, first, lineStyle.horizontalSeparator)
      columns.columns.tail.map { x =>
        sb.append(lineStyle.horizontalVerticalSeparator)
        _fill(sb, x, lineStyle.horizontalSeparator)
      }
    }
    sb.append(lineStyle.rightHorizontalSeparator)
    sb.append(newline)
  }

  private def _fill(sb: StringBuilder, column: ColumnInfo, p: String) {
    for (i <- 0 until column.width by lineStyle.unitWidth) {
      sb.append(p)
    }
  }

  private def _draw_row(sb: StringBuilder, columns: ColumnInfos, row: Row) {
    val matrix = row.matrix
    _draw_row_line(sb, columns, matrix, 0)
    if (!isCompact)
      for (y <- 1 until matrix.height) {
        _draw_row_line(sb, columns, matrix, 1)
      }
  }

  private def _draw_row_line(
    sb: StringBuilder,
    columns: ColumnInfos,
    matrix: IMatrix[String],
    y: Int
  ) {
    _left_border(sb)
    _cell(sb, columns(0), matrix(0, y))
    for (x <- 1 until matrix.width) {
      _vertical_separator(sb)
      _cell(sb, columns(x), matrix(x, y))
    }
    _right_border(sb)
    _newline(sb)
  }

  private def _cell(sb: StringBuilder, c: ColumnInfo, p: String): Unit = {
    lineStyle.unitWidth match {
      case 1 => _cell_1(sb, c, p)
      case 2 => _cell_2(sb, c, p)
      case n => RAISE.noReachDefect
    }
  }

  private def _cell_1(sb: StringBuilder, c: ColumnInfo, p: String) = {
    // TODO left/center/right
    val sw = stringWidth(p)
    val n = math.max(c.width - sw, 0)
    sb.append(p)
    for (i <- 0 until n)
      sb.append(' ')
  }

  private def _cell_2(sb: StringBuilder, c: ColumnInfo, p: String) = {
    // TODO left/center/right
    val sw = stringWidth(p)
    val n0 = math.max(c.width - sw, 0)
    val n = n0 % 2 match {
      case 0 => c.width
      case 1 => c.width + 1
    }
    sb.append(p)
    for (i <- 0 until n)
      sb.append(' ')
  }

  private def _top_left_corner(sb: StringBuilder) = {
    sb.append(lineStyle.topLeftCorner)
  }

  private def _top_right_corner(sb: StringBuilder) = {
    sb.append(lineStyle.topRightCorner)

  }

  private def _top_border(sb: StringBuilder) = {
    sb.append(lineStyle.topBorder)
  }

  private def _top_separator(sb: StringBuilder) = {
    sb.append(lineStyle.topSeparator)
  }

  private def _bottom_left_corner(sb: StringBuilder) = {
    sb.append(lineStyle.bottomLeftCorner)
  }

  private def _bottom_right_corner(sb: StringBuilder) = {
    sb.append(lineStyle.bottomRightCorner)

  }

  private def _bottom_border(sb: StringBuilder) = {
    sb.append(lineStyle.bottomBorder)
  }

  private def _bottom_separator(sb: StringBuilder) = {
    sb.append(lineStyle.bottomSeparator)
  }

  private def _left_border(sb: StringBuilder) = {
    sb.append(lineStyle.leftBorder)
  }

  private def _right_border(sb: StringBuilder) = {
    sb.append(lineStyle.rightBorder)
  }

  private def _vertical_separator(sb: StringBuilder) {
    sb.append(lineStyle.verticalSeparator)
  }

  private def _left_horizontal_separator(sb: StringBuilder) {
    sb.append(lineStyle.leftHorizontalSeparator)
  }

  private def _right_horizontal_separator(sb: StringBuilder) {
    sb.append(lineStyle.rightHorizontalSeparator)
  }

  private def _horizontal_separator(sb: StringBuilder) {
    sb.append(lineStyle.horizontalSeparator)
  }

  private def _horizontal_vertical_separator(sb: StringBuilder) {
    sb.append(lineStyle.horizontalVerticalSeparator)
  }

  private def _newline(sb: StringBuilder) {
    sb.append(newline)
  }

  // def stringWidth(p: String): Int = p.map(charWidth).sum
  // def charWidth(p: Char): Int = if (p > 0x1000) 2 else 1 // TODO
  private def stringWidth(p: String): Int = StringUtils.stringConsoleWidth(p)
  private def charWidth(p: Char): Int = StringUtils.charConsoleWidth(p)
}
object MatrixVisualizer {
  sealed trait LineStyle extends NamedValueInstance {
    def unitWidth: Int = verticalSeparator.length
    def topLeftCorner: String
    def topRightCorner: String
    def topBorder: String
    def topSeparator: String
    def bottomLeftCorner: String
    def bottomRightCorner: String
    def bottomBorder: String
    def bottomSeparator: String
    def middleLeft: String = leftHorizontalSeparator
    def middleRight: String = rightHorizontalSeparator
    def middleSeparator: String = horizontalSeparator
    def middleVerticalSeparator: String = horizontalVerticalSeparator
    def leftBorder: String
    def rightBorder: String
    def verticalSeparator: String
    def leftHorizontalSeparator: String
    def rightHorizontalSeparator: String
    def horizontalSeparator: String
    def horizontalVerticalSeparator: String
  }
  object LineStyle extends EnumerationClass[LineStyle] {
    val elements = Vector(SpaceLineStyle, CommaLineStyle, AsciiLineStyle, JisLineStyle, LinearAlgebraStyle)
  }
  case object SpaceLineStyle extends LineStyle {
    val name = "space"

    def topLeftCorner: String = ""
    def topRightCorner: String = ""
    def topBorder: String = ""
    def topSeparator: String = ""
    def bottomLeftCorner: String = ""
    def bottomRightCorner: String = ""
    def bottomBorder: String = ""
    def bottomSeparator: String = ""
    def leftBorder: String = ""
    def rightBorder: String = ""
    def verticalSeparator: String = " "
    def leftHorizontalSeparator: String = ""
    def rightHorizontalSeparator: String = ""
    def horizontalSeparator: String = " "
    def horizontalVerticalSeparator: String = " "
  }
  case object CommaLineStyle extends LineStyle {
    val name = "comma"

    def topLeftCorner: String = ""
    def topRightCorner: String = ""
    def topBorder: String = ""
    def topSeparator: String = ""
    def bottomLeftCorner: String = ""
    def bottomRightCorner: String = ""
    def bottomBorder: String = ""
    def bottomSeparator: String = ""
    def leftBorder: String = ""
    def rightBorder: String = ""
    def verticalSeparator: String = ","
    def leftHorizontalSeparator: String = ""
    def rightHorizontalSeparator: String = ""
    def horizontalSeparator: String = " "
    def horizontalVerticalSeparator: String = " "
  }
  case object AsciiLineStyle extends LineStyle {
    val name = "ascii"

    def topLeftCorner: String = "+"
    def topRightCorner: String = "+"
    def topBorder: String = "-"
    def topSeparator: String = "+"
    def bottomLeftCorner: String = "+"
    def bottomRightCorner: String = "+"
    def bottomBorder: String = "-"
    def bottomSeparator: String = "+"
    def leftBorder: String = "|"
    def rightBorder: String = "|"
    def verticalSeparator: String = "|"
    def leftHorizontalSeparator: String = "+"
    def rightHorizontalSeparator: String = "+"
    def horizontalSeparator: String = "-"
    def horizontalVerticalSeparator: String = "+"
  }
  case object JisLineStyle extends LineStyle {
    val name = "jis"

    def topLeftCorner: String = "┌"
    def topRightCorner: String = "┐"
    def topBorder: String = "─"
    def topSeparator: String = "┬"
    def bottomLeftCorner: String = "└"
    def bottomRightCorner: String = "┘"
    def bottomBorder: String = "─"
    def bottomSeparator: String = "┴"
    def leftBorder: String = "│"
    def rightBorder: String = "│"
    def verticalSeparator: String = "│"
    def leftHorizontalSeparator: String = "├"
    def rightHorizontalSeparator: String = "┤"
    def horizontalSeparator: String = "─"
    def horizontalVerticalSeparator: String = "┼"
  }
  case object JisThickLineStyle extends LineStyle {
    val name = "jis-thick"

    def topLeftCorner: String = "┏"
    def topRightCorner: String = "┓"
    def topBorder: String = "━"
    def topSeparator: String = "┯" // "┳"
    def bottomLeftCorner: String = "┗"
    def bottomRightCorner: String = "┛"
    def bottomBorder: String = "━"
    def bottomSeparator: String = "┷" // "┻"
    override def middleLeft: String = "┣"
    override def middleRight: String = "┫"
    override def middleSeparator: String = "━"
    override def middleVerticalSeparator: String = "┿"
    def leftBorder: String = "┃"
    def rightBorder: String = "┃"
    def verticalSeparator: String = "│"
    def leftHorizontalSeparator: String = "┠"
    def rightHorizontalSeparator: String = "┨"
    def horizontalSeparator: String = "─"
    def horizontalVerticalSeparator: String = "┼"
  }
  case object LinearAlgebraStyle extends LineStyle {
    val name = "linearAlgebra"

    def topLeftCorner: String = "┌"
    def topRightCorner: String = "┐"
    def topBorder: String = " "
    def topSeparator: String = " "
    def bottomLeftCorner: String = "└"
    def bottomRightCorner: String = "┘"
    def bottomBorder: String = " "
    def bottomSeparator: String = " "
    def leftBorder: String = "│"
    def rightBorder: String = "│"
    def verticalSeparator: String = " "
    def leftHorizontalSeparator: String = " "
    def rightHorizontalSeparator: String = " "
    def horizontalSeparator: String = " "
    def horizontalVerticalSeparator: String = " "
  }

  sealed trait BorderStyle extends NamedValueInstance {
  }
  object BorderStyle extends EnumerationClass[BorderStyle] {
    val elements = Vector(NoneBorder, TopEndBorder, BottomEndBorder, MiddleBorder)
  }
  case object NoneBorder extends BorderStyle {
    val name = "none"
  }
  case object TopEndBorder extends BorderStyle {
    val name = "top-end"
  }
  case object BottomEndBorder extends BorderStyle {
    val name = "bottom-end"
  }
  case object MiddleBorder extends BorderStyle {
    val name = "middle"
  }

  sealed trait Align extends NamedValueInstance {
  }
  object Align extends EnumerationClass[Align] {
    val elements = Vector(Left, Center, Right)

    case object Left extends Align {
      val name ="left"
    }
    case object Center extends Align {
      val name ="center"
    }
    case object Right extends Align {
      val name ="right"
    }
  }

  case class ColumnDef(
    width: Option[Int]
  ) {
    def withWidth(p: Int) = copy(width = Some(p))
    def withoutWidth() = copy(width = None)
  }
  object ColumnDef {
    val empty = ColumnDef(None)
  }
  case class ColumnDefs(columns: IndexedSeq[ColumnDef]) {
    def length = columns.length
    def apply(i: Int): ColumnDef = get(i).getOrElse(ColumnDef.empty)
    def get(i: Int): Option[ColumnDef] = columns.lift(i)
  }
  object ColumnDefs {
    val empty = ColumnDefs(Vector.empty)
  }
  case class ColumnInfo(
    width: Int,
    align: Option[Align] = None
  ) {
    def withWidth(p: Int) = copy(width = p)
  }
  case class ColumnInfos(columns: IndexedSeq[ColumnInfo]) {
    def length = columns.length
    def width = 1 + columns.map(x => x.width + 1).sum
    def apply(i: Int): ColumnInfo = columns(i)
  }
  case class Rows(rows: IndexedSeq[Row]) {
    lazy val width = rows.map(_.width).max

    def headOption = rows.headOption
    def tailWithIndex = rows.tail.zipWithIndex
  }
  case class Row(cells: IndexedSeq[Cell]) {
    def width = cells.length

    def matrix: IMatrix[String] = VectorColumnRowMatrix(cells.map(_.lines), "")

    def effectiveCells(p: ColumnInfos): IndexedSeq[Cell] =
      if (cells.length >= p.length)
        cells
      else
        (cells.toStream ++ Stream.continually(Cell.empty)).take(p.length).toVector
  }
  case class Cell(lines: Seq[String]) {
    def normalize(p: ColumnInfo, align: Align): Cell = (p.align getOrElse align) match {
      case Align.Left => Cell(lines.map(AnyUtils.toEmbed(_, p.width)))
      case Align.Center => Cell(lines.map(AnyUtils.toEmbedCenter(_, p.width)))
      case Align.Right => Cell(lines.map(AnyUtils.toEmbedRight(_, p.width)))
    }

    def normalizex(p: ColumnInfo, f: Char => Int) = Cell(lines.flatMap(_normalize(p.width, f, _)))

    private def _normalize(width: Int, f: Char => Int, p: String): Vector[String] = {
      case class Z(
        lines: Vector[String] = Vector.empty,
        current: Vector[Char] = Vector.empty,
        count: Int = 0
      ) {
        def r = if (current.isEmpty)
          lines
        else
          lines :+ current.mkString

        def +(rhs: Char) = {
          val w = f(rhs)
          val newcount = count + w
          if (newcount > width)
            Z(lines :+ (current :+ rhs).mkString, Vector.empty, 0)
          else
            copy(current = current :+ rhs, count = newcount)
        }
      }
      p./:(Z())(_+_).r
    }
  }
  object Cell {
    val empty = Cell(Vector.empty)

    def apply(p: String): Cell = Cell(Vector(p))
  }

  def linearAlgebra[T] = MatrixVisualizer(TopEndBorder, BottomEndBorder, true, true, false, LinearAlgebraStyle, "\n", (_: ColumnDef, v: T) => v.toString)

  def border[T](f: (ColumnDef, T) => String) = MatrixVisualizer(TopEndBorder, BottomEndBorder, true, true, true, AsciiLineStyle, "\n", f)
  def header[T](f: (ColumnDef, T) => String) = MatrixVisualizer(TopEndBorder, MiddleBorder, true, true, true, AsciiLineStyle, "\n", f)
  def body[T](f: (ColumnDef, T) => String) = MatrixVisualizer(NoneBorder, NoneBorder, true, true, true, AsciiLineStyle, "\n", f)
  def bodyStart[T](f: (ColumnDef, T) => String) = MatrixVisualizer(TopEndBorder, NoneBorder, true, true, true, AsciiLineStyle, "\n", f)
  def bodyEnd[T](f: (ColumnDef, T) => String) = MatrixVisualizer(NoneBorder, BottomEndBorder, true, true, true, AsciiLineStyle, "\n", f)
  def footer[T](f: (ColumnDef, T) => String) = MatrixVisualizer(MiddleBorder, BottomEndBorder, true, true, true, AsciiLineStyle, "\n", f)

  // def compact(width: Int, p: ColumnDefs): ColumnDefs = compact2(width, p)

  // def compact1(width: Int, p: ColumnDefs): ColumnDefs = {
  //   RAISE.notImplementedYetDefect
  // }

  // def compact2(width: Int, p: ColumnDefs): ColumnDefs = {
  //   val linewidth = 2
  //   val avgwidth = _half_up((width / p.length) - 2)

  //   case class Z(xs: Vector[ColumnDef] = Vector.empty, count: Int = linewidth) {
  //     def r = {
  //       val remainder = width - count
  //       if (remainder <= 0) {
  //         xs.map(x => if (x.width.isDefined) x else x.withWidth(avgwidth))
  //       } else {
  //         val undefinedcount = xs.count(_.width.isDefined == false)
  //         val basewidth = remainder / undefinedcount
  //         val fraction = remainder % undefinedcount
  //         case class ZZ(r: Vector[ColumnDef] = Vector.empty, i: Int = fraction) {
  //           def +(rhs: ColumnDef) =
  //             if (rhs.width.isDefined) {
  //               copy(r = r :+ rhs)
  //             } else {
  //               if (i > 0) {
  //                 val w = basewidth + linewidth
  //                 copy(r = r :+ rhs.withWidth(w), i = i - linewidth)
  //               } else {
  //                 copy(r = r :+ rhs.withWidth(basewidth))
  //               }
  //             }
  //         }
  //         xs./:(ZZ())(_+_).r
  //       }
  //     }

  //     def +(rhs: ColumnDef) = {
  //       def _withwidth(w: Int) =
  //         if (w <= avgwidth)
  //           _set(w)
  //         else
  //           _count(avgwidth)

  //       def _withoutwidth() = _count(avgwidth)

  //       def _set(w: Int) = copy(xs = xs :+ rhs.withWidth(w), count = count + w + linewidth)

  //       def _count(w: Int) = copy(xs = xs :+ rhs.withoutWidth(), count = count + w + linewidth)

  //       rhs.width.map(_withwidth).getOrElse(_withoutwidth)
  //     }
  //   }
  //   val r = p.columns./:(Z())(_+_).r
  //   ColumnDefs(r)
  // }

  // private def _half_up(p: Int) = if (p % 2 == 0) p else p - 1
}
