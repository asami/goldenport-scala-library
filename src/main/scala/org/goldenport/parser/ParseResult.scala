package org.goldenport.parser

import scalaz._, Scalaz._  
import scala.util.control.NonFatal
import java.net.{URI, URL}
import java.io.File
import org.goldenport.exception.SyntaxErrorFaultException
  
/*
 * @since   Aug. 21, 2018
 *  version Nov. 18, 2018
 *  version Dec.  2, 2018
 *  version Feb.  2, 2019
 *  version Jul. 21, 2019
 *  version Sep. 29, 2020
 *  version Oct. 14, 2020
 * @version Jan. 10, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait ParseResult[+AST] {
  def map[T](p: AST => T): ParseResult[T] = this.asInstanceOf[ParseResult[T]]
  // ParseResult is not Monad. Just to use 'for' comprehension in Scala syntax suger.
  def flatMap[T](f: AST => ParseResult[T]): ParseResult[T]
  def get: Option[AST] = toOption
  def take: AST = get getOrElse {
    throw new IllegalArgumentException(messages.mkString(";"))
  }
  def errors: Vector[ErrorMessage]
  def warnings: Vector[WarningMessage]
  def messages: Vector[ParseMessage] = errors ++ warnings
  def toOption: Option[AST]

  def append(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST]
  def prepend(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST]
}

case class EmptyParseResult[AST]() extends ParseResult[AST] {
  def flatMap[T](f: AST => ParseResult[T]): ParseResult[T] = this.asInstanceOf[ParseResult[T]]
  def errors = Vector.empty
  def warnings = Vector.empty
  def toOption: Option[AST] = None
  def append(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] = this.asInstanceOf[ParseResult[AST]]
  def prepend(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] = this.asInstanceOf[ParseResult[AST]]
}

case class ParseSuccess[AST](
  ast: AST,
  warnings: Vector[WarningMessage] = Vector.empty
) extends ParseResult[AST] {
  override def map[T](p: AST => T): ParseResult[T] = ParseSuccess[T](p(ast), warnings)
  def flatMap[T](f: AST => ParseResult[T]): ParseResult[T] = f(ast).prepend(Nil, warnings)
  def errors = Vector.empty
  def toOption: Option[AST] = Some(ast)
  def append(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] = copy(warnings = warnings ++ ws)
  def prepend(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] = copy(warnings = ws.toVector ++ warnings)
}

case class ParseFailure[AST](
  errors: Vector[ErrorMessage],
  warnings: Vector[WarningMessage]
) extends ParseResult[AST] {
  def flatMap[T](f: AST => ParseResult[T]): ParseResult[T] = this.asInstanceOf[ParseResult[T]]
  def toOption: Option[AST] = None

  def append(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] =
    copy(errors = errors ++ es, warnings = warnings ++ ws)
  def prepend(es: Seq[ErrorMessage], ws: Seq[WarningMessage]): ParseResult[AST] =
    copy(errors = es.toVector ++ errors, warnings = ws.toVector ++ warnings)

  def withLocation(p: URI): ParseFailure[AST] = copy(
    errors = errors.map(_.withLocation(p)),
    warnings = warnings.map(_.withLocation(p))
  )
  def withLocation(p: URL): ParseFailure[AST] = withLocation(p.toURI)
  def withLocation(p: File): ParseFailure[AST] = withLocation(p.toURI)

  def complementLocation(p: URI): ParseFailure[AST] = copy(
    errors = errors.map(_.complementLocation(p)),
    warnings = warnings.map(_.complementLocation(p))
  )
  def complementLocation(p: URL): ParseFailure[AST] = complementLocation(p.toURI)
  def complementLocation(p: File): ParseFailure[AST] = complementLocation(p.toURI)

  def message = errors.map(_.msg.en).mkString(";")

  def RAISE = throw SyntaxErrorFaultException(this)
}
object ParseFailure {
  def apply[AST](msg: String): ParseFailure[AST] = apply(msg, msg, None)

  def apply[AST](msg: String, location: Option[ParseLocation]): ParseFailure[AST] = apply(msg, msg, location)

  def apply[AST](en: String, ja: String, location: Option[ParseLocation]): ParseFailure[AST] =
    ParseFailure(Vector(ErrorMessage(en, ja, location)), Vector.empty)

  def apply[AST](e: Throwable): ParseFailure[AST] = ParseFailure(Vector(ErrorMessage(e)), Vector.empty)
}

object ParseResult {
  implicit object ParseResultApplicative extends Applicative[ParseResult] {
    def ap[A, B](fa: => ParseResult[A])(f: => ParseResult[A => B]): ParseResult[B] = f match {
      case ParseSuccess(sf, cs) => fa.map(sf).append(Nil, cs)
      case m: ParseFailure[_] => fa match {
        case mm: ParseSuccess[_] => m.append(mm.errors, mm.warnings).asInstanceOf[ParseFailure[B]]
        case mm: ParseFailure[_] => mm.append(m.errors, m.warnings).asInstanceOf[ParseFailure[B]]
        case mm: EmptyParseResult[_] => m.asInstanceOf[EmptyParseResult[B]]
      }
      case m: EmptyParseResult[_] => m.asInstanceOf[EmptyParseResult[B]]
    }
    def point[A](a: => A): ParseResult[A] = ParseSuccess(a)
  }

  def apply[AST](p: => AST): ParseResult[AST] = execute(p)
  def execute[AST](p: => AST): ParseResult[AST] = try {
    ParseSuccess(p)
  } catch {
    case NonFatal(e) => ParseFailure(e)
  }

  def execute[AST](msg: => String)(p: => AST): ParseResult[AST] = try {
    ParseSuccess(p)
  } catch {
    case NonFatal(e) => ParseFailure(msg)
  }

  def execute[AST](msg: Throwable => String)(p: => AST): ParseResult[AST] = try {
    ParseSuccess(p)
  } catch {
    case NonFatal(e) => ParseFailure(msg(e))
  }

  def executeDebug[AST](msg: => String)(p: => AST): ParseResult[AST] = try {
    ParseSuccess(p)
  } catch {
    case NonFatal(e) => throw e
  }

  def empty[AST] = EmptyParseResult[AST]()

  def success[AST](p: AST): ParseSuccess[AST] = ParseSuccess(p)

  def error[AST](msg: String): ParseFailure[AST] = ParseFailure(msg)

  def error[AST](en: String, ja: String, location: Option[ParseLocation]): ParseFailure[AST] =
    ParseFailure(en, ja, location)

  def error[AST](e: Throwable): ParseFailure[AST] = ParseFailure(e)

  def orMissing[AST](msg: String, p: Option[AST]): ParseResult[AST] =
    p.map(success).getOrElse(error(msg))
}
