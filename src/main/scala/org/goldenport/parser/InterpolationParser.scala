package org.goldenport.parser

import org.goldenport.util.StringUtils

/*
 * @since   Sep. 21, 2019
 *  version Sep. 27, 2019
 * @version Oct. 11, 2019
 * @author  ASAMI, Tomoharu
 */
case class InterpolationParser(
  needParenthesis: Boolean = false
) {
  import InterpolationParser._

  def apply(p: CharSequence): Stream[Either[String, ScriptToken]] =
    if (p.length == 0)
      Stream.empty
    else
      _get_first(p).map(_parse).getOrElse(Stream(Left(p.toString)))

  private def _get_first(p: CharSequence): Option[Cont] =
    if (needParenthesis)
      _get_first_parenthesis(p)
    else
      _get_first_dollar(p)

  private def _get_first_dollar(p: CharSequence): Option[Cont] = {
    var dollarposition: Option[Int] = None
    var afterdollar: Boolean = false
    for (i <- 0 until p.length - 1) {
      def nextchar = if (i >= p.length) null else p.charAt(i + 1)
      p.charAt(i) match {
        case '$' =>
          if (afterdollar) {
            dollarposition = None
            afterdollar = false
          } else if (nextchar != '$') {
            dollarposition = Some(i)
          }
        case _ => // do nothing
      }
      dollarposition.foreach(x =>
        return if (x == 0)
          Some(Cont(p, Stream.empty))
        else
          Some(Cont(p.subSequence(x, p.length), Stream(Left(p.subSequence(0, x).toString))))
      )
    }
    None
  }

  private def _get_first_parenthesis(p: CharSequence): Option[Cont] = {
    var dollarposition: Option[Int] = None
    var isopen = false
    var isclose = false
    for (i <- 0 until p.length) {
      def nextchar = if (i >= p.length) null else p.charAt(i + 1)
      p.charAt(i) match {
        case '$' => if (nextchar != '$') dollarposition = Some(i)
        case '{' => isopen = true
        case '}' => isclose = true
        case _ => // do nothing
      }
      dollarposition.foreach(x =>
        if (isopen && isclose) {
          return if (x == 0)
            Some(Cont(p, Stream.empty))
          else
            Some(Cont(p.subSequence(x, p.length), Stream(Left(p.subSequence(0, x).toString))))
        }
      )
    }
    None
  }

  private def _parse(c: Cont): Stream[Either[String, ScriptToken]] =
    c.result.toStream #::: _parse(c.source)

  private def _parse(p: CharSequence): Stream[Either[String, ScriptToken]] = {
    val c = _simple_parse(p)
    // println(s"c = $c")
    if (c.source.length == 0)
      c.result
    else
      c.result #::: _token_parse(c.source)
  }

  private def _simple_parse(p: CharSequence): Cont =
    if (p.length <= 1)
      Cont.end(p)
    else
      _simple_parse_option(p).getOrElse(Cont(p))

  private def _simple_parse_option(p: CharSequence): Option[Cont] = {
    var state = NEUTRAL
    var startpos: Int = -1
    var propertystartpos: Int = -1
    val properties = new StringBuilder
    var expression: String = null
    val postfix = new StringBuilder

    def prologue = p.subSequence(0, startpos).toString
    def getproperties = if (properties.isEmpty) None else Some(properties.toString)
    def getpostfix = if (postfix.isEmpty) None else Some(postfix.toString)

    for (i <- 0 until p.length) {
      val c = p.charAt(i)
      def nextchar = if (i + 1 >= p.length) null else p.charAt(i + 1)
      state match {
        case NEUTRAL =>
          if (c == '$') {
            nextchar match {
              case '{' => 
                state = CANDIDATE
                startpos = i
              case '[' =>
                state = PROPERTY_CANDIDATE
                propertystartpos = i + 1
              case _ => state = SHORT_IN
            }
          } else {
            state = NEUTRAL
          }
        case CANDIDATE =>
          if (c == '{') {
            state = FIRST
          } else {
            state = NEUTRAL
          }
        case FIRST =>
          if (c == '}') { // exceptional case
            val a = if (startpos == 0)
              Some(Cont.prologue(p, i))
            else
              None
            return a
          } else if (StringUtils.isScriptIdentifierFirstChar(c)) {
            state = IN
          } else {
            if (startpos == 0) {
              return None
            } else {
              return Some(Cont(p.subSequence(startpos, p.length), Stream(Left(prologue))))
            }
          }
        case IN =>
          if (c == '}') {
            if (nextchar == '%') {
              expression = p.subSequence(startpos + 2, i).toString
              state = POSTFIX_CANDIDATE
            } else {
              val script = ScriptToken(p.subSequence(startpos + 2, i).toString, getproperties, None)
              val source = p.subSequence(i + 1, p.length)
              val cont = _simple_parse_option(source) match {
                case Some(s) =>
                  if (startpos == 0) {
                    Cont(s.source, Right(script) #:: s.result)
                  } else {
                    Cont(s.source, Stream(Left(prologue), Right(script)) #::: s.result)
                  }
                case None =>
                  if (startpos == 0) {
                    Cont(source, Stream(Right(script)))
                  } else {
                    Cont(source, Stream(Left(prologue), Right(script)))
                  }
              }
              return (Some(cont))
            }
          } else if (StringUtils.isScriptIdentifierChar(c)) {
            // println(s"scriptidentifier:$c")
            state = IN
          } else {
            if (startpos == 0) {
              return None
            } else {
              return Some(Cont(p.subSequence(startpos, p.length), Stream(Left(prologue))))
            }
          }
        case SHORT_IN =>
          if (c == '%') {
            expression = p.subSequence(startpos + 2, i).toString
            state = POSTFIX_CANDIDATE
          } else if (StringUtils.isScriptIdentifierChar(c)) {
            state = SHORT_IN
          } else {
            val script = ScriptToken(p.subSequence(startpos + 2, i).toString, getproperties, None)
            val source = p.subSequence(i, p.length)
            val cont = _cont(prologue, script, source)
            return (Some(cont))
          }
        case PROPERTY_CANDIDATE => state = PROPERTY_IN
        case PROPERTY_IN =>
          if (c == ']') {
            state = CANDIDATE
            startpos = i
          } else {
            properties.append(p.charAt(i))
          }
        case POSTFIX_CANDIDATE =>
          if (nextchar == '{') {
            state = POSTFIX_IN_CANDIDATE
          } else {
            postfix.append(c)
            state = POSTFIX_SHORT_IN
          }
        case POSTFIX_IN_CANDIDATE => state = POSTFIX_IN
        case POSTFIX_IN =>
          if (c == '}') {
            val script = ScriptToken(expression, getproperties, getpostfix)
            val source = p.subSequence(i + 1, p.length)
            val cont = _cont(prologue, script, source)
            return (Some(cont))
          } else {
            postfix.append(c)
          }
        case POSTFIX_SHORT_IN =>
          if (c == ' ') {
            val script = ScriptToken(expression, getproperties, getpostfix)
            val source = p.subSequence(i, p.length)
            val cont = _cont(prologue, script, source)
            return (Some(cont))
          } else {
            postfix.append(c)
          }
      }
    }
    state match {
      case SHORT_IN => expression = p.subSequence(startpos + 2, p.length).toString
      case _ => // do nothing
    }
    if (expression == null)
      Some(Cont.end(p))
    else
      Some(Cont.end(ScriptToken(expression, getproperties, getpostfix)))
  }

  private def _cont(prologue: String, script: ScriptToken, source: CharSequence): Cont =
    _simple_parse_option(source) match {
      case Some(s) =>
        if (prologue.isEmpty) {
          Cont(s.source, Right(script) #:: s.result)
        } else {
          Cont(s.source, Stream(Left(prologue), Right(script)) #::: s.result)
        }
      case None =>
        if (prologue.isEmpty) {
          Cont(source, Stream(Right(script)))
        } else {
          Cont(source, Stream(Left(prologue), Right(script)))
        }
    }

  private def _token_parse(p: CharSequence) = {
    val tokens = LogicalTokens.parseWithoutLocation(p.toString)
    // println(s"tokens: $p => $tokens")
    case class Z(
      result: Vector[Either[String, ScriptToken]] = Vector.empty,
      buffer: StringBuilder = new StringBuilder()
    ) {
      def r = (result ++ _to_string).toStream

      private def _to_string: Vector[Either[String, ScriptToken]] =
        if (buffer.isEmpty)
          Vector.empty
        else
          Vector(Left(buffer.toString))

      def +(rhs: LogicalToken) = rhs match {
        case m: ScriptToken => Z(result ++ _to_string :+ Right(m), new StringBuilder())
        case m => copy(buffer = buffer.append(m.raw))
      }
    }
    tokens.tokens./:(Z())(_+_).r
  }
}

object InterpolationParser {
  private final val NEUTRAL = 0
  private final val CANDIDATE = 1
  private final val FIRST = 2
  private final val IN = 3
  private final val SHORT_IN = 4
  private final val PROPERTY_CANDIDATE = 5
  private final val PROPERTY_IN = 6
  private final val POSTFIX_CANDIDATE = 7
  private final val POSTFIX_IN_CANDIDATE = 8
  private final val POSTFIX_IN = 9
  private final val POSTFIX_SHORT_IN = 10

  private case class Cont(source: CharSequence, result: Stream[Either[String, ScriptToken]]) {
    // def headOption = result.headOption
    // def tail = copy(result = result.tail)
//    def remainderString: String = source.subSequence(c.i, c.source.length).toString
    // def promote(i: Int) = Cont(source.subSequence(i, source.length), result :+ Left(source.subSequence(0, i).toString))
    // def resultStream = result.toStream
  }
  private object Cont {
    def apply(p: CharSequence): Cont = Cont(p, Stream.empty)

    def end(p: CharSequence): Cont =
      if (p.length == 0)
        Cont("", Stream.empty)
      else
        Cont("", Stream(Left(p.toString)))

    def end(p: ScriptToken): Cont = Cont("", Stream(Right(p)))

    def prologue(p: CharSequence, i: Int): Cont =
      Cont(p.subSequence(i, p.length), Stream(Left(p.subSequence(0, i).toString)))
  }

  def parse(p: CharSequence): Stream[Either[String, ScriptToken]] =
    InterpolationParser()(p)
}
