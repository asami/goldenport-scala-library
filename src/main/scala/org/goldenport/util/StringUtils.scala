package org.goldenport.util

import scalaz.NonEmptyList
import scala.util.control.NonFatal
import java.net.{URL, URI}
import java.net.URLEncoder
import com.asamioffice.goldenport.text.{UString, UPathString}
import org.goldenport.Strings
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
 * @version Dec. 17, 2017
 * @author  ASAMI, Tomoharu
 */
object StringUtils {
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

  def concatPath(path: Seq[String]): String = {
    path.length match {
      case 0 => ""
      case 1 => path.head
      case _ => path.tail.foldLeft(path.head)(concatPath).toString
    }
  }

  def isSuffix(s: String, suffix: String): Boolean = {
    UPathString.isSuffix(s, suffix)
  }

  def isSuffix(s: String, suffix: Seq[String]): Boolean = {
    UPathString.isSuffix(s, suffix.toArray)
  }

  def isSuffix(s: String, suffix: Set[String]): Boolean =
    suffix.contains(toSuffix(s))

  def toSuffix(s: String): String = UPathString.getSuffix(s)

  def getSuffix(s: String): Option[String] = Option(UPathString.getSuffix(s))

  def toPathnameBody(s: String): String = UPathString.getPathnameBody(s)

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

  /*
   * Int
   */
  def intOrString(s: String): Either[Int, String] = try {
    Left(s.toInt)
  } catch {
    case NonFatal(e) => Right(s)
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
}
