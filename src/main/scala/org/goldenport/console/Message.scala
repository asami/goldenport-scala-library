package org.goldenport.console

/*
 * @since   Jan. 10, 2021
 *  version Jan. 17, 2021
 * @version Mar.  8, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Message {
  def text: String
}

object Message {
  case class Decoration(
    blink: Boolean = false,
    bold: Boolean = false,
    invisible: Boolean = false,
    reverse: Boolean = false,
    underline: Boolean = false,
    foreground: Option[Decoration.Color] = None,
    background: Option[Decoration.Color] = None
  ) {
    def withBlink(p: Boolean = true) = copy(blink = true)
    def withBold(p: Boolean = true) = copy(bold = true)
    def withUnderline(p: Boolean = true) = copy(underline = true)
    def withBackground(p: Decoration.Color) = copy(background = Some(p))

    def apply(p: String): String = {
      val prefix = new StringBuilder()
      if (underline)
        prefix.append(Console.UNDERLINED)
      if (bold)
        prefix.append(Console.BOLD)
      if (invisible)
        prefix.append(Console.INVISIBLE)
      if (reverse)
        prefix.append(Console.REVERSED)
      if (underline)
        prefix.append(Console.UNDERLINED)

      (foreground, background) match {
        case (Some(fg), Some(bg)) =>
          prefix.append(fg.foreground)
          prefix.append(bg.background)
        case (Some(fg), None) =>
          prefix.append(fg.foreground)
          prefix.append(fg.background)
        case (None, Some(bg)) =>
          prefix.append(bg.background)
        case (None, None) => // do nothing
      }
      if (prefix.isEmpty) {
        p
      } else {
        prefix.append(p)
        prefix.append(Console.RESET)
        prefix.toString
      }
    }
  }
  object Decoration {
    sealed trait Color {
      def foreground: String
      def background: String
    }
    case object Black extends Color {
      def foreground: String = Console.BLACK
      def background: String = Console.WHITE_B
    }
    case object Red extends Color {
      def foreground: String = Console.RED
      def background: String = Console.WHITE_B
    }
    case object Green extends Color {
      def foreground: String = Console.GREEN
      def background: String = Console.WHITE_B
    }
    case object Yellow extends Color {
      def foreground: String = Console.YELLOW
      def background: String = Console.BLACK_B
    }
    case object Blue extends Color {
      def foreground: String = Console.BLUE
      def background: String = Console.WHITE_B
    }
    case object Magenta extends Color {
      def foreground: String = Console.MAGENTA
      def background: String = Console.WHITE_B
    }
    case object Cyan extends Color {
      def foreground: String = Console.CYAN
      def background: String = Console.BLACK_B
    }
    case object White extends Color {
      def foreground: String = Console.WHITE
      def background: String = Console.BLACK_B
    }

    val black = Decoration(foreground = Some(Black))
    val red = Decoration(foreground = Some(Red))
    val green = Decoration(foreground = Some(Green))
    val yellow = Decoration(foreground = Some(Yellow))
    val blue = Decoration(foreground = Some(Blue))
    val magenta = Decoration(foreground = Some(Magenta))
    val cyan = Decoration(foreground = Some(Cyan))
    val white = Decoration(foreground = Some(White))
    val bold = Decoration(bold = true)
    val underline = Decoration(underline = true)
    val blink = Decoration(blink = true)
    val reverse = Decoration(reverse = true)
  }

  def apply(p: String): PlainMessage = PlainMessage(p)

  def prompt(p: String): Prompt = Prompt(p)
  def black(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.black)
  def red(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.red)
  def green(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.green)
  def yellow(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.yellow)
  def blue(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.blue)
  def magenta(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.magenta)
  def cyan(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.cyan)
  def white(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.white)
  def bold(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.bold)
  def underline(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.underline)
  def blink(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.blink)
  def reverse(p: String): DecoratedMessage = DecoratedMessage(p, Decoration.reverse)
}

case class Prompt(prompt: String) extends Message {
  def text = prompt
}

case class PlainMessage(message: String) extends Message {
  def text = message
}

case class DecoratedMessage(message: String, decoration: Message.Decoration) extends Message {
  def text = message
  def withUnderline(p: Boolean = true) = copy(decoration = decoration.withUnderline(p))
  def withBlink(p: Boolean = true) = copy(decoration = decoration.withBlink(p))
  def withBold(p: Boolean = true) = copy(decoration = decoration.withBold(p))
}

case class ErrorMessage(message: String) extends Message {
  def text = message
}

case class WarningMessage(message: String) extends Message {
  def text = message
}
