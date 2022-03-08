package org.goldenport.util

import scalaz.NonEmptyList
import scala.util.Try
import scala.util.control.NonFatal
import Character.UnicodeBlock
import java.net.{URL, URI}
import java.net.URLEncoder
import com.asamioffice.goldenport.text.{UString, UPathString}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.StringFormatter
import org.goldenport.values.{PathName, Urn}

/*
 * @since   May. 24, 2014
 *  version Jul. 19, 2014
 *  version Dec. 26, 2014
 *  version Jan.  1, 2015
 *  version Oct. 24, 2015
 *  version Dec. 25, 2015
 *  version Mar. 31, 2016
 *  version Apr. 29, 2016
 *  version Jun.  9, 2016
 *  version Jul. 15, 2016
 *  version Feb. 24, 2017
 *  version Jul. 29, 2017
 *  version Aug. 29, 2017
 *  version Sep. 28, 2017
 *  version Nov. 14, 2017
 *  version Dec. 17, 2017
 *  version Jan. 14, 2018
 *  version May. 30, 2018
 *  version Aug. 31, 2018
 *  version Oct. 10, 2018
 *  version Feb. 14, 2019
 *  version Mar.  5, 2019
 *  version May. 19, 2019
 *  version Jul. 29, 2019
 *  version Sep. 15, 2019
 *  version Nov. 28, 2019
 *  version Dec.  5, 2019
 *  version Jan. 27, 2020
 *  version Mar. 18, 2020
 *  version May.  4, 2020
 *  version Jul. 29, 2020
 *  version Sep.  1, 2020
 *  version Jan.  9, 2021
 *  version Apr. 10, 2021
 *  version Dec. 31, 2021
 *  version Feb. 25, 2022
 * @version Mar.  6, 2022
 * @author  ASAMI, Tomoharu
 */
object StringUtils {
  // RFC-3986
  val uriReservedChars = Vector(
    '!', '#', '$', '&', '\'', '(', ')', '*', '+', ',', '/', ':',
    ';', '=', '?', '@', '[', ']'
  )

  val uriAvailableSpecialChars = Vector(
    '-', '.', '_', '~'
  )

  val uriSafeSpecialChars = Vector(
    '-', '_'
  )

  val unsafeChars = Vector(
    // char -> byte
    '\u00ab',
    '\u00af',
    '\u00b5',
    '\u00b7',
    '\u00b8',
    '\u00bb',
    '\u2014',
    '\u2015',
    '\u2016',
    '\u2212',
    '\u2225',
    '\u301c', // 〜 0xFF5E
    '\u3094',
    '\uff02',
    '\uff07',
    '\uff0d',
    '\uff5e',
    '\uffe0',
    '\uffe1',
    '\uffe2',
    '\uffe4',
    // byte -> char
    '\u815c',
    '\u8160',
    '\u8161',
    '\u817c',
    '\u8191',
    '\u8192',
    '\u81ca',
    // Java round trip
    '\u00a5',
    '\u203e',
    // Windows 31J
    '\u00a2',
    '\u00a3',
    '\u00a5',
    '\u00ab',
    '\u00ac',
    '\u00af',
    '\u00b5',
    '\u00b7',
    '\u00b8',
    '\u00bb',
    '\u203e',
    '\u3094',
    // Shift_JIS
    '\u00a2',
    '\u00a3',
    '\u00a5',
    '\u00ac',
    '\u2014',
    '\u2016',
    '\u203e',
    '\u2212',
    '\u301c'
  ).distinct

  val numericalSymbols = Vector(
    '+', '*', '-', '/', '=', '&', '|'
  )

  val numberSymbols = Vector(
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '+', '-', '.', ','
  )

  // TODO
  val scriptSymbolChars = Vector(
    '.', ',',
    '(', ')', '{', '}', '[', ']',
    '$', '@'
  ) ++ numericalSymbols

  // TODO
  val lispSymbolChars = Vector(
    '.', ',',
    '(', ')', '{', '}', '[', ']',
    '$', '@'
  ) ++ numericalSymbols

  val safeJapaneseCharBlocks = Vector(
    UnicodeBlock.HIRAGANA,
    UnicodeBlock.KATAKANA,
    UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS
  )

  // FUTURE
  val safeI18NCharBlocks = safeJapaneseCharBlocks ++ Vector(
  )

  val symbolCharBlocks = Vector(
    UnicodeBlock.CJK_SYMBOLS_AND_PUNCTUATION,
    UnicodeBlock.GEOMETRIC_SHAPES,
    UnicodeBlock.GENERAL_PUNCTUATION,
    UnicodeBlock.SPECIALS,
    UnicodeBlock.BOX_DRAWING,
    UnicodeBlock.MISCELLANEOUS_SYMBOLS,
    UnicodeBlock.ARROWS,
    UnicodeBlock.MATHEMATICAL_OPERATORS
  )

  val unsafeIdentifierCharBlocks = Vector(
    UnicodeBlock.HALFWIDTH_AND_FULLWIDTH_FORMS,
    UnicodeBlock.CJK_SYMBOLS_AND_PUNCTUATION,
    UnicodeBlock.GEOMETRIC_SHAPES,
    UnicodeBlock.GENERAL_PUNCTUATION,
    UnicodeBlock.SPECIALS,
    UnicodeBlock.BOX_DRAWING,
    UnicodeBlock.MISCELLANEOUS_SYMBOLS,
    UnicodeBlock.ARROWS,
    UnicodeBlock.LATIN_1_SUPPLEMENT,
    UnicodeBlock.MATHEMATICAL_OPERATORS
    // UnicodeBlock.GREEK
  )

  def isAsciiAlphabetString(s: String) = s.forall(isAsciiAlphabetChar)
  def isAsciiAlphabetNumberString(s: String) = s.forall(isAsciiAlphabetNumberChar)
  def isAsciiNumberString(s: String) = s.forall(isAsciiNumberChar)
  def isAsciiString(s: String) = s.forall(isAsciiChar)

  def isAsciiChar(c: Char) = c <= 0xff
  def isSafeAsciiChar(c: Char) = c != 0x5c && (0x20 <= c && c <= 0x7e)
  def isAsciiAlphabetChar(c: Char) =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  def isAsciiNumberChar(c: Char) = '0' <= c && c <= '9'
  def isAsciiAlphabetNumberChar(c: Char) =
    isAsciiAlphabetChar(c) || isAsciiNumberChar(c)

  def isUriAvailableAlphabetNumberChar(c: Char) =
    isAsciiAlphabetChar(c) || isAsciiNumberChar(c)

  def isUriAvailableChar(c: Char) = {
    isUriAvailableAlphabetNumberChar(c) ||
    uriAvailableSpecialChars.contains(c)
  }

  def isSafeUriChar(c: Char) = {
    isUriAvailableAlphabetNumberChar(c) ||
    uriSafeSpecialChars.contains(c)
  }

  // http://d.hatena.ne.jp/cero-t/20100204/1265302329
  // http://www.bugbearr.jp/?Java/%E6%96%87%E5%AD%97%E3%82%B3%E3%83%BC%E3%83%89
  def isUnsafeChar(c: Char) = unsafeChars.contains(c)

  def isNumericalSymbol(s: String) = s.forall(isNumericalSymbolChar)

  def isNumericalSymbolChar(c: Char) = numericalSymbols.contains(c)

  def isNumber(p: String): Boolean =
    p.equalsIgnoreCase("true") || p.equalsIgnoreCase("false") || p.forall(isNumberSymbolChar)

  def isNumberWide(p: String): Boolean = isNumber(p) || _is_complicated_number(p)

  private def _is_complicated_number(p: String) = false // TODO 0.0L, +3e5

  def isNumberSymbolChar(c: Char) = numberSymbols.contains(c)

  def isI18NIdentifier(s: String) =
    if (s.isEmpty)
      false
    else
      s.forall(isI18NIdentifierChar)

  def isI18NIdentifierChar(c: Char) =
    safeI18NCharBlocks.contains(UnicodeBlock.of(c))

  // JEXL
  def isScriptIdentifier(s: String) = s.headOption.
    map(x => isScriptIdentifierFirstChar(x) && s.tail.forall(isScriptIdentifierChar)).
    getOrElse(false)

  def isScriptIdentifierI18N(s: String) = isScriptIdentifier(s) || isI18NIdentifier(s)


  def isScriptIdentifierChar(c: Char) = isScriptIdentifierFirstChar(c) || isAsciiNumberChar(c)

  def isScriptIdentifierFirstChar(c: Char) = isAsciiAlphabetChar(c) || c == '_' || c == '$'

  def isScriptIdentifierI18NChar(c: Char) = isScriptIdentifierChar(c) || isI18NIdentifierChar(c)

  def isScriptIdentifierI18NFirstChar(c: Char) = isScriptIdentifierFirstChar(c) || isI18NIdentifierChar(c)

  def isScriptSymbolChar(c: Char) = scriptSymbolChars.contains(c)

  // Lisp
  def isLispIdentifier(s: String) = s.headOption.
    map(x => isLispIdentifierFirstChar(x) && s.tail.forall(isLispIdentifierChar)).
    getOrElse(false)

  def isLispIdentifierI18N(s: String) = isLispIdentifier(s) || isI18NIdentifier(s)

  def isLispIdentifierChar(c: Char) = isLispIdentifierFirstChar(c) || isAsciiNumberChar(c) || c == '-'

  def isLispIdentifierFirstChar(c: Char) = isAsciiAlphabetChar(c) || c == '_'

  def isLispIdentifierI18NChar(c: Char) = isLispIdentifierChar(c) || isI18NIdentifierChar(c)

  def isLispIdentifierI18NFirstChar(c: Char) = isLispIdentifierFirstChar(c) || isI18NIdentifierChar(c)

  def isLispSymbolChar(c: Char) = lispSymbolChars.contains(c)

  def stringConsoleWidth(p: String): Int = p.map(charConsoleWidth).sum

  def charConsoleWidth(p: Char): Int = if (p > 0x1000) 2 else 1 // TODO

  //
  def dropWhileRight(s: String, p: Char => Boolean): String =
    s.lastOption.map(l =>
      if (p(l))
        s.dropRight(s.reverse.takeWhile(p).length)
      else
        s
    ).getOrElse(s)

  def dropRightNewlines(s: String): String = dropWhileRight(s, x => x == '\n' || x == '\r')

  // Ignore head '/' in rhs
  def concatPath(lhs: String, rhs: String): String = {
    (lhs, rhs) match {
      case ("", "") => ""
      case (l, "") => lhs
      case ("", r) => rhs
      case (l, r) => 
        (lhs.endsWith("/"), rhs.startsWith("/")) match {
          case (true, true) => lhs + rhs.tail
          case (true, false) => lhs + rhs
          case (false, true) => lhs + rhs
          case (false, false) => lhs + "/" + rhs
        }
    }
  }

  // def concatPath(path: Seq[String]): String = {
  //   path.length match {
  //     case 0 => ""
  //     case 1 => path.head
  //     case _ => path.tail.foldLeft(path.head)(concatPath).toString
  //   }
  // }

  def concatPath(path: Seq[String]): String = concatPath('/', path)

  def concatPath(delimiter: Char, path: Seq[String]): String = {
    def _concat_path_(lhs: String, rhs: String) = s"${lhs}${delimiter}${escapePathComponent(delimiter, rhs)}"
    path.filterNot(_.isEmpty).length match {
      case 0 => ""
      case 1 => escapePathComponent(delimiter, path.head)
      case _ => path.tail.foldLeft(path.head)(_concat_path_)
    }
  }

  def escapePathComponent(delimiter: Char, p: String): String = {
    val sb = new StringBuilder()
    for (c <- p) {
      if (c == delimiter)
        sb.append("\\")
        sb.append(c)
    }
    sb.toString
  }

  def isSuffix(s: String, suffix: String): Boolean = {
    // UPathString.isSuffix(s, suffix)
    isSuffix(s, Set(suffix))
  }

  def isSuffix(s: String, suffix: Seq[String]): Boolean = {
    // UPathString.isSuffix(s, suffix.toArray)
    isSuffix(s, suffix.toSet)
  }

  def isSuffix(s: String, suffix: Set[String]): Boolean =
    suffix.contains(toSuffix(s))

  def toSuffix(s: String): String = getSuffix(s).getOrElse("")

  def getSuffix(s: String): Option[String] = Option(UPathString.getSuffix(s)).map(_.toLowerCase)

  def getSuffixLowerCase(s: String): Option[String] = getSuffix(s)

  def getSuffixRaw(s: String): Option[String] = Option(UPathString.getSuffix(s))

  def toPathnameBody(s: String): String = UPathString.getPathnameBody(s)

  def pathnameBodySuffix(p: String): (String, Option[String]) = {
    (toPathnameBody(p), Option(UPathString.getSuffix(p)))
  }

  def pathnameBodySuffixLowered(p: String): (String, Option[String]) =
    (toPathnameBody(p), getSuffix(p))

  def dimString(s: String, length: Int = 1000): String = {
    val postfix = "..."
    val postfixlength = postfix.length
    if (s == null)
      ""
    else if (s.isEmpty)
      ""
    else if (s.length <= postfixlength)
      s(0) +: postfix
    else if (s.length <= length)
      s.substring(0, (s.length / 2)) + postfix
    else
      s.substring(0, (length - postfixlength)) + postfix
  }

  // TODO migrate Strings
  def safeString(s: Option[String]): String = {
    if (s == null)
      ""
    else
      s getOrElse ""
  }
  def safeString(s: String): String = if (s == null) "" else s

  def pathLastComponent(path: String): String = {
    UPathString.getLastComponent(path)
  }

  def pathLastComponentBody(path: String): String = {
    UPathString.getLastComponentBody(path)
  }

  def pathContainer(path: String): String = {
    UPathString.getContainerPathname(path)
  }

  def pathRelative(root: String, path: String): String = {
    val r = if (root.endsWith("/")) root else s"${root}/"
    if (path.startsWith(r))
      path.substring(r.length)
    else
      path
  }

  def pathRelativeBody(root: String, path: String): String = toPathnameBody(pathRelative(root, path))

  def className(o: Object): String =  {
    val a1 = o.getClass.getSimpleName
    if (a1.endsWith("$"))
      a1.substring(0, a1.length - 1)
    else
      a1
  }

  def classNameToHypenName(suffix: String, o: Object): String = {
    val a = className(o)
    if (a.endsWith(suffix))
      camelToHyphen(a.substring(0, a.length - suffix.length))
    else
      camelToHyphen(a)
  }

  def camelToHyphen(s: String): String = {
    s.foldLeft(InitState)(_.apply(_)).result
  }

  val InitState: ParseState = NeutralState()

  sealed trait ParseState {
    def result: String
    def apply(rhs: Char): ParseState
    protected def to_string(xs: Vector[String]) = xs.map(_.toLowerCase).mkString("-")
  }
  case class NeutralState(
    z: Vector[String] = Vector.empty
  ) extends ParseState {
    def apply(rhs: Char) = InWordState(z, Vector(rhs))
    def result: String = to_string(z)
  }
  case class InWordState(
    z: Vector[String],
    x: Vector[Char]
  ) extends ParseState {
    def apply(rhs: Char) = {
      if (rhs.isUpper)
        copy(z :+ x.mkString, Vector(rhs))
      else
        copy(z, x :+ rhs)
    }

    def result: String = to_string(z :+ x.mkString)
  }

  def objectToUnderscoreName(postfix: String, o: Object): String =
    classToUnderscoreName(postfix, o.getClass)

  def classToUnderscoreName(postfix: String, klass: Class[_]): String = {
    val name = {
      val n = klass.getSimpleName
      if (n.endsWith("$"))
        n.substring(0, n.length - 1)
      else
        n
    }
    camelToUnderscore(postfix, name)
  }

  def camelToUnderscore(postfix: String, name: String): String =
    if (name.toLowerCase.endsWith(postfix.toLowerCase))
      camelToUnderscore(name.substring(0, name.length - postfix.length))
    else
      camelToUnderscore(name)

  def camelToUnderscore(p: String): String =
    p.foldLeft(camel.InitState("_"))(_.apply(_)).result

  object camel {
    def InitState(d: String): ParseState = NeutralState(d)

    sealed trait ParseState {
      def delimiter: String
      def result: String
      def apply(rhs: Char): ParseState
      protected def to_string(xs: Vector[String]) = xs.map(_.toLowerCase).mkString(delimiter)
    }
    case class NeutralState(
      delimiter: String,
      z: Vector[String] = Vector.empty
    ) extends ParseState {
      def apply(rhs: Char) = InWordState(delimiter, z, Vector(rhs))
      def result: String = to_string(z)
    }
    case class InWordState(
      delimiter: String,
      z: Vector[String],
      x: Vector[Char]
    ) extends ParseState {
      def apply(rhs: Char) = {
        if (rhs.isUpper)
          copy(delimiter, z :+ x.mkString, Vector(rhs))
        else
          copy(delimiter, z, x :+ rhs)
      }

      def result: String = to_string(z :+ x.mkString)
    }
  }

  def addUrlParam(path: String, key: String, value: String): String =
    addUrlParams(path, Vector(key -> value))

  def addUrlParams(path: String, params: Seq[(String, String)]): String = {
    if (params.isEmpty)
      return path
    val buf = new StringBuilder
    def f(k: String, v: String) {
      buf.append(URLEncoder.encode(k, "UTF-8"))
      buf.append('=')
      buf.append(URLEncoder.encode(v, "UTF-8"))
    }
    buf.append(path)
    if (path.contains('?'))
      buf.append('&')
    else
      buf.append('?')
    val (k, v) = params.head
    f(k, v)
    for ((k, v) <- params.tail) {
      buf.append('&')
      f(k, v)
    }
    buf.toString
  }

  def urlQuery(key: String, value: String): String = {
    s"""${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"""
  }

  def urlQueryString(p: Seq[(String, String)]): String =
    p.map {
      case (k, v) => urlQuery(k, v)
    }.mkString("&")

  // def addUrlParams(path: String, params: Record): String =
  //   addUrlParams(path, params.toVector.map {
  //     case (k, v) => k -> AnyUtils.toString(v)
  //   })

  def shortPathName(p: String): String = shortPathName(PathName(p))
  def shortPathName(p: PathName): String = p.components match {
    case Nil => ""
    case x :: Nil => x
    case xs => xs.init.map(_.headOption.getOrElse("")).mkString + "/" + xs.last
  }
  def shortUri(p: String): String = shortUri(new URI(p))
  def shortUri(p: URI): String = {
    val host = Option(p.getHost)
    val path = Option(p.getPath)
    val shortpath = path.map(shortPathName)
    (host, shortpath) match {
      case (None, None) => ""
      case (Some(h), None) => h
      case (None, Some(sp)) => sp
      case (Some(h), Some(sp)) => s"$h:$sp"
    }
  }
  def shortUrl(p: URL): String = shortUri(p.toURI)
  def shortUrn(p: Urn): String = p.text

  def normalizeBaseUrl(p: String): String =
    if (p.endsWith("/"))
      p
    else
      p + "/"

  def print(p: String, newline: String): String = {
    val a = Strings.tolines(p)
    val c = a.mkString(newline)
    c
  }

  def printConsole(p: String, newline: String, size: Int = 5): String = {
    val a = Strings.tolines(p)
    val c = a.take(size).mkString(newline)
    if (a.length > size)
      s"$c ... (${a.length}/${p.length})"
    else
      c
  }

  def showConsole(p: String, newline: String, size: Int = 5): String = {
    val width = 80
    val a = Strings.tolines(p)
    if (a.length == 1)
      _show_console_one_line(a(0), newline, size, width)
    else
      _show_console_lines(p, a, newline, size, width)
  }

  private def _show_console_one_line(p: String, newline: String, size: Int, width: Int) =
    Strings.cutstring(p, width * 5 - 10)

  private def _show_console_lines(p: String, a: Vector[String], newline: String, size: Int, width: Int) = {
    val b = a.map(Strings.cutstring(_, width))
    val c = b.take(size).mkString(newline)
    if (a.length > size)
      s"$c$newline ... total: ${a.length} lines/${p.length} characters"
    else
      c
  }

  def normalizeConsoleMessage(newline: String)(p: String): String =
    p.replace("\r\n", newline).replace("\r", newline).replace("\n", newline)

  def normalizeConsoleMessageWithTrailingNewline(newline: String)(p: String): String = {
    val a = normalizeConsoleMessageWithoutTrailingNewline(newline)(p)
    if (a.isEmpty)
      a
    else
      s"$a$newline"
  }

  def normalizeConsoleMessageWithoutTrailingNewline(newline: String)(p: String): String =
    dropRightNewlines(normalizeConsoleMessage(newline)(p))

  def forToString(p: String): String = showConsole(p, "\\n", 1)

  def toEmbedConsole(p: String, width: Int): String =
    StringFormatter.display.embed(p, width)

  /*
   * List
   */
  def eagerCommaForm(p: String): List[String] =
    Strings.totokens(p, ",").map(_.trim).filter(Strings.notblankp)

  def eagerCommaForm(ps: List[String]): List[String] = ps.flatMap(eagerCommaForm)

  /*
   * Int
   */
  def intOrString(s: String): Either[Int, String] = try {
    Left(s.toInt)
  } catch {
    case NonFatal(e) => Right(s)
  }

  def intOption(p: String): Option[Int] =
    if (p.isEmpty) {
      None
    } else {
      val s = p.trim
      val c = s(0)
      if (c == '+' || c == '-' || ('0' <= c && c <= '9')) {
        try {
          Some(s.toInt)
        } catch {
          case NonFatal(e) => None
        }
      } else {
        None
      }
    }

  def longOption(p: String): Option[Long] =
    if (p.isEmpty) {
      None
    } else {
      val s = p.trim
      val c = s(0)
      if (c == '+' || c == '-' || ('0' <= c && c <= '9')) {
        try {
          Some(s.toLong)
        } catch {
          case NonFatal(e) => None
        }
      } else {
        None
      }
    }

  def doubleOption(s: String): Option[Double] =
    if (s.isEmpty)
      None
    else
      numberOption(s).filter(_is_double).map(_.doubleValue)

  private def _is_double(p: Any) = p match {
    case _: Byte => true
    case _: Short => true
    case _: Int => true
    case _: Long => true
    case _: Float => true
    case _: Double => true
    case _: BigInt => false
    case _: BigDecimal => false
    case _ => false
  }

  def numberOption(p: String): Option[Number] =
    if (p.isEmpty) {
      None
    } else {
      val s = p.trim
      val length = s.length
      val c = s(0)
      if (c == '+' || c == '-' || ('0' <= c && c <= '9')) {
        if (s.contains('.') || s.contains('e') || s.contains('E')) {
          Try(s.toDouble: Number).toOption.orElse(Try(new java.math.BigDecimal(s)).toOption)
        } else {
          if (length > 11)
            Try(s.toLong: Number).toOption.orElse(Try(new java.math.BigInteger(s)).toOption)
          else if (length > 9)
            Try(s.toInt: Number).toOption.orElse(Try(s.toLong: Number).toOption)
          else
            Try(s.toInt: Number).toOption
        }
      } else if (c == '.') {
        if (length > 1) {
          val c1 = s(1)
          if ('0' <= c1 && c1 <= '9')
            Try(s.toDouble: Number).toOption.orElse(Try(BigDecimal(s)).toOption)
          else
            None
        } else {
          None
        }
      } else {
        None
      }
    }

  def getMarkInt(p: String): Option[(String, Int)] = {
    val (name, number) = p.span(x => !isAsciiNumberChar(x))
    if (name.isEmpty || number.isEmpty)
      None
    else
      intOption(number).map(x => name -> x)
  }

  def numberToKanjiCharacterSequence(p: Int): String =
    if (p == 0) {
      "零"
    } else {
      val sb = new StringBuilder()
      @annotation.tailrec
      def go(x: Int): String = {
        if (x == 0) {
          sb.toString
        } else {
          val v = p / 10
          val r = p % 10
          sb.append(toKanjiCharacter(r))
          go(v)
        }
      }
      go(p)
    }

  def toKanjiCharacter(p: Int): Character = p match {
    case 0 => '零'
    case 1 => '一'
    case 2 => '二'
    case 3 => '三'
    case 4 => '四'
    case 5 => '五'
    case 6 => '六'
    case 7 => '七'
    case 8 => '八'
    case 9 => '九'
    case _ => RAISE.invalidArgumentFault(s"No digit number: $p")
  }

  /*
   * Display
   */
  def label(l: String, name: String): String = {
    label(Option(l), name)
  }

  def label(l: Option[String], name: String): String = {
    Strings.blankopt(l) getOrElse UString.capitalize(name)
  }

  /*
   * LTSV
   *
   * Migrate from com.everforth.everforth.util.StringUtils
   * Migrate to org.goldenport.util.StringUtils
   */
  def tokeyvalue(s: String, ds: String = ":"): (String, String) = {
    val i = s.indexOf(ds)
    if (i == -1) (s, "")
    else (s.substring(0, i).trim, s.substring(i + 1))
  }

  def ltsv2seq(s: String): Seq[(String, String)] = {
    Strings.totokens(s, "\t") map { x =>
      tokeyvalue(x)
    }
  }

  def ltsv2map(s: String): Map[String, String] = {
    val a = Strings.totokens(s, "\t") map { x =>
      tokeyvalue(x)
    }
    Map.empty ++ a
  }

  def normalizeLtsvLine(s: String): String = {
    @annotation.tailrec
    def clearlastnewlines(s: String): String = {
      s.lastOption match {
        case Some(c) if c == '\n' || c == '\r' => clearlastnewlines(s.init)
        case _ => s
      }
    }
    if (s.contains("\n") | s.contains("\r")) {
      val s1 = clearlastnewlines(s)
      s1.replace("\n", """\n""").replace("\r", """\r""")
    } else {
      s
    }
  }

  def makeOptionNonEmptyListString(s: String): Option[NonEmptyList[String]] = {
    ScalazUtils.makeOptionNonEmptyList(Strings.totokens(s).toList)
  }

  // def marshallStringsForCsv(xs: Seq[String]): String = {
  //   xs.toList match {
  //     case Nil => ""
  //     case x :: Nil => x
  //     case xs => JsonUtils.data2json(xs)
  //   }
  // }

  // def unmarshallStringsForCsv(s: String): Vector[String] = {
  //   val a = s.trim
  //   if (a.startsWith("[") && a.endsWith("]")) {
  //     Json.parse(a) match {
  //       case xs: JsArray => RecordUtils.js2strings(xs)
  //       case x => throw new IllegalArgumentException(s"Not js array: $s")
  //     }
  //   } else {
  //     Vector(s)
  //   }
  // }

  // Stack
  def makeStack(e: Throwable): String = {
    val StackSize = 4096
//    val StackSize = 2048

    // hilight a stack part (not exception message)
    try {
      val s = new java.io.StringWriter
      val p = new java.io.PrintWriter(s)
      e.printStackTrace(p)
      Strings.cutstring(s.toString, StackSize).replace('\t', ' ') // cut for logging
    } catch {
      case NonFatal(e) => "Crash in getting exception information = " + e.getMessage
    }
  }

  def makeStackFull(e: Throwable): String = {
    try {
      val s = new java.io.StringWriter
      val p = new java.io.PrintWriter(s)
      e.printStackTrace(p)
      s.toString.replace('\t', ' ')
    } catch {
      case NonFatal(e) => "Crash in getting exception information = " + e.getMessage
    }
  }

  private var _stack_advice_count = 0
  def makeStackAdvice(e: Throwable): String = {
    if (_stack_advice_count > 100)
      "Stack advice count exceeds threshold: " + _stack_advice_count
    else {
      val a = makeStack(e)
      val index = a.indexOf("at ")
      if (index == -1) a
      else a.substring(index)
    }
  }

  // Option
  def unmarshallOption[T](p: Option[String], f: String => T): Option[T] =
    p.flatMap(unmarshallOption(_, f))

  def unmarshallOption[T](p: String, f: String => T): Option[T] =
    Strings.blankopt(p).map(f)

  def prefixRemainder(p: String, prefix: String): Option[String] =
    if (p.startsWith(prefix))
      Some(p.substring(prefix.length))
    else
      None
}
