package org.goldenport.context

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}
import org.goldenport.parser.{ParseMessage}

/*
 * See org.goldenport.record.v2.ConclusionResult.
 * 
 * @since   Feb. 21, 2021
 *  version May. 30, 2021
 *  version Jun. 20, 2021
 *  version Oct. 25, 2021
 *  version Nov. 30, 2021
 *  version Dec.  5, 2021
 *  version Jan. 30, 2022
 * @version Mar. 10, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait Consequence[+T] {
  def conclusion: Conclusion
  def toOption: Option[T]
  def add(p: Conclusion): Consequence[T]
  def map[U](f: T => U): Consequence[U]
  // Consequence is not Monad. Just to use 'for' comprehension in Scala syntax suger.
  def flatMap[U](f: T => Consequence[U]): Consequence[U]
  def forConfig: Consequence[T]

  // def getMessage: Option[String] = conclusion.getMessage
  def message: String = conclusion.message
  // def getMessage(locale: Locale): Option[String] = conclusion.getMessage(locale)
  def message(locale: Locale): String = conclusion.message(locale)

  def isSuccess: Boolean = conclusion.isSuccess

  def get = toOption

  def take: T

  def takeOrInvalidArgumentFault(message: String): T = get getOrElse Conclusion.invalidArgumentFault(message).RAISE

  def takeOrIllegalConfigurationDefect(name: String): T = get getOrElse Conclusion.config.illegalConfigurationDefect(name).RAISE
}

object Consequence {
  case class Success[+T](
    result: T,
    conclusion: Conclusion = Conclusion.Ok
  ) extends Consequence[T] {
    def toOption: Option[T] = Some(result)
    def add(p: Conclusion): Consequence[T] = copy(conclusion = conclusion + p)
    def map[U](f: T => U): Consequence[U] = copy(result = f(result))
    def flatMap[U](f: T => Consequence[U]): Consequence[U] = f(result).add(conclusion)
    def take = result
    def forConfig: Consequence[T] = if (conclusion.isSuccess) this else copy(conclusion = conclusion.forConfig)
  }

  case class Error[+T](
    conclusion: Conclusion = Conclusion.InternalServerError
  ) extends Consequence[T] {
    def toOption: Option[T] = None
    def add(p: Conclusion): Consequence[T] = copy(conclusion = conclusion + p)
    def map[U](f: T => U): Consequence[U] = this.asInstanceOf[Error[U]]
    def flatMap[U](f: T => Consequence[U]): Consequence[U] = this.asInstanceOf[Consequence[U]]
    def take = RAISE
    def forConfig: Consequence[T] = if (conclusion.isSuccess) this else copy(conclusion = conclusion.forConfig)

    def RAISE: Nothing = throw new ConsequenceException(this)
  }

  implicit object ConsequenceApplicative extends Applicative[Consequence] {
    def ap[A, B](fa: => Consequence[A])(f: => Consequence[A => B]): Consequence[B] = f match {
      case Success(sf, cs) => fa.map(sf).add(cs)
      case m: Error[_] => fa match {
        case mm: Success[_] => m.add(mm.conclusion).asInstanceOf[Error[B]]
        case mm: Error[_] => mm.add(m.conclusion).asInstanceOf[Error[B]]
      }
    }
    def point[A](a: => A): Consequence[A] = Success(a)
  }

  def apply[T](p: => T): Consequence[T] = execute(p)
  def success[T](p: T): Consequence[T] = Success(p)

  // Generic error derived from HTTP
  def badRequest[T](p: String): Consequence[T] = badRequest(I18NString(p))
  def badRequest[T](p: I18NString): Consequence[T] = error(400, p)
  def unauthorized[T](p: String): Consequence[T] = unauthorized(I18NString(p))
  def unauthorized[T](en: String, ja: String): Consequence[T] = unauthorized(I18NString(en, ja))
  def unauthorized[T](p: I18NString): Consequence[T] = error(401, p)
  def paymentRequired[T](p: String): Consequence[T] = paymentRequired(I18NString(p))
  def paymentRequired[T](p: I18NString): Consequence[T] = error(402, p)
  def forbidden[T](p: String): Consequence[T] = forbidden(I18NString(p))
  def forbidden[T](p: I18NString): Consequence[T] = error(403, p)
  def notFound[T](p: String): Consequence[T] = notFound(I18NString(p))
  def notFound[T](p: I18NString): Consequence[T] = error(404, p)
  def methodNotAllowed[T](p: String): Consequence[T] = methodNotAllowed(I18NString(p))
  def methodNotAllowed[T](p: I18NString): Consequence[T] = error(405, p)
  def notAcceptable[T](p: String): Consequence[T] = notAcceptable(I18NString(p))
  def notAcceptable[T](p: I18NString): Consequence[T] = error(406, p)
  def proxyAuthenticationRequired[T](p: String): Consequence[T] = proxyAuthenticationRequired(I18NString(p))
  def proxyAuthenticationRequired[T](p: I18NString): Consequence[T] = error(407, p)
  def requestTimeout[T](p: String): Consequence[T] = requestTimeout(I18NString(p))
  def requestTimeout[T](p: I18NString): Consequence[T] = error(408, p)
  def conflict[T](p: String): Consequence[T] = conflict(I18NString(p))
  def conflict[T](p: I18NString): Consequence[T] = error(409, p)
  def gone[T](p: String): Consequence[T] = gone(I18NString(p))
  def gone[T](p: I18NString): Consequence[T] = error(410, p)
  def internalServerError[T](p: String): Consequence[T] = internalServerError(I18NString(p))
  def internalServerError[T](p: I18NString): Consequence[T] = error(500, p)
  def notImplemented[T](p: String): Consequence[T] = notImplemented(I18NString(p))
  def notImplemented[T](p: I18NString): Consequence[T] = error(501, p)
  def badGateway[T](p: String): Consequence[T] = badGateway(I18NString(p))
  def badGateway[T](p: I18NString): Consequence[T] = error(502, p)
  def serviceUnavailable[T](p: String): Consequence[T] = serviceUnavailable(I18NString(p))
  def serviceUnavailable[T](p: I18NString): Consequence[T] = error(503, p)
  def gatewayTimeout[T](p: String): Consequence[T] = gatewayTimeout(I18NString(p))
  def gatewayTimeout[T](p: I18NString): Consequence[T] = error(504, p)

  //
  def error[T](code: Int, p: String): Consequence[T] = error(code, I18NString(p))
  def error[T](code: Int, p: I18NString): Consequence[T] = Error(Conclusion.error(code, p))
  def error[T](e: Throwable): Consequence[T] = e match {
    case m: IllegalArgumentException => badRequest(m.getMessage)
    case m: SecurityException => unauthorized(m.getMessage)
    case m: UnsupportedOperationException => notImplemented(m.getMessage)
    case m: NoSuchElementException => notFound(m.getMessage)
    case m: java.io.FileNotFoundException => notFound(m.getMessage)
    case m => internalServerError(m.getMessage)
  }

  //
  def successOrMissingPropertyFault[T](name: String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(missingPropertyFault(name))
  // def successOrMissingPropertyOrError[T](name: String, p: Option[Left[String, T]]): Consequence[T] =
  //   p.map(success).getOrElse(missingPropertyOrError(name))

  // Specific error with detail code.

  def successOrInvalidTokenFault[T](name: String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(invalidTokenFault(name))

  def invalidArgumentFault[T](p: String): Consequence[T] = invalidArgumentFault(I18NMessage(p))
  def invalidArgumentFault[T](p: String, arg: Any, args: Any*): Consequence[T] = Error(Conclusion.invalidArgumentFault(I18NMessage(p, args +: args)))
  def invalidArgumentFault[T](p: I18NMessage): Consequence[T] = Error(Conclusion.invalidArgumentFault(p))

  def missingArgumentFault[T](p: String, ps: String*): Consequence[T] = missingArgumentFault(p +: ps)
  def missingArgumentFault[T](ps: Seq[String]): Consequence[T] = Error(Conclusion.missingArgumentFault(ps))

  def invalidPropertyFault[T](p: String): Consequence[T] = invalidPropertyFault(I18NMessage(p))
  def invalidPropertyFault[T](p: String, arg: Any, args: Any*): Consequence[T] = Error(Conclusion.invalidPropertyFault(I18NMessage(p, args +: args)))
  def invalidPropertyFault[T](p: I18NMessage): Consequence[T] = Error(Conclusion.invalidPropertyFault(p))

  def missingPropertyFault[T](p: String, ps: String*): Consequence[T] = missingPropertyFault(p +: ps)
  def missingPropertyFault[T](ps: Seq[String]): Consequence[T] = Error(Conclusion.missingPropertyFault(ps))

  def invalidTokenFault[T](value: String): Consequence[T] = Error(Conclusion.invalidTokenFault(value))
  def invalidTokenFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.invalidTokenFault(label, value))

  def valueDomainFault[T](value: String): Consequence[T] = Error(Conclusion.valueDomainFault(value))
  def valueDomainFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.valueDomainFault(label, value))

  def syntaxErrorFault[T](message: String): Consequence[T] = Error(Conclusion.syntaxErrorFault(message))

  def syntaxErrorFault[T](messages: Seq[Message]): Consequence[T] = Error(Conclusion.syntaxErrorFault(messages))

  //
  def execute[T](body: => T): Consequence[T] = try {
    Success(body)
  } catch {
    case NonFatal(e) => error(e)
  }

  def executeOrMissingPropertyFault[T](name: String)(p: => Option[T]): Consequence[T] =
    execute(p).flatMap {
      case Some(s) => Success(s)
      case None => Error(Conclusion.missingPropertyFault(name))
    }

  def run[T](body: => Consequence[T]): Consequence[T] = try {
    body
  } catch {
    case NonFatal(e) => error(e)
  }

  //
  def from[A](p: ParseResult[A]): Consequence[A] = p match {
    case m: ParseSuccess[_] => Success(m.ast, _conclusion_success(m))
    case m: ParseFailure[_] => Error(_conclusion_error(m))
    case m: EmptyParseResult[_] => Error(_conclusion_error(m))
  }

  private def _conclusion_success(p: ParseResult[_]): Conclusion =
    Conclusion(
      StatusCode.Ok,
      None,
      errors = _errors(p.errors),
      warnings = _warnings(p.warnings)
    )

  private def _conclusion_error(p: ParseResult[_]): Conclusion = 
    Conclusion(
      StatusCode.SyntaxError,
      None,
      errors = _errors(p.errors),
      warnings = _warnings(p.warnings)
    )

  private def _conclusion_config_error(p: ParseResult[_]): Conclusion = 
    Conclusion(
      StatusCode.Config,
      None,
      errors = _errors(p.errors),
      warnings = _warnings(p.warnings)
    )

  private def _errors(ps: Vector[ErrorMessage]): ErrorMessages = ErrorMessages(ps)

  private def _warnings(ps: Vector[WarningMessage]): WarningMessages = WarningMessages(ps)

  def from[E <: Fault, A](p: ValidationNel[E, A]): Consequence[A] = p match {
    case scalaz.Success(s) => Consequence.success(s)
    case scalaz.Failure(es) => Consequence.Error(Conclusion.make(es))
  }

  object config {
    def successOrMissingPropertyFault[T](name: String, p: Option[T]): Consequence[T] =
      p.map(success).getOrElse(missingPropertyFault(name))
  // def successOrMissingPropertyOrError[T](name: String, p: Option[Left[String, T]]): Consequence[T] =
  //   p.map(success).getOrElse(missingPropertyOrError(name))

  // Specific error with detail code.

    def successOrInvalidTokenFault[T](name: String, p: Option[T]): Consequence[T] =
      p.map(success).getOrElse(invalidTokenFault(name))

    def invalidPropertyFault[T](p: String): Consequence[T] = Error(Conclusion.config.invalidPropertyFault(p))

    def missingPropertyFault[T](p: String): Consequence[T] = Error(Conclusion.config.missingPropertyFault(p))

    def invalidTokenFault[T](value: String): Consequence[T] = Error(Conclusion.config.invalidTokenFault(value))

    def invalidTokenFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.config.invalidTokenFault(label, value))

    def valueDomainFault[T](value: String): Consequence[T] = Error(Conclusion.config.valueDomainFault(value))

    def valueDomainFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.config.valueDomainFault(label, value))

    def illegalConfigurationDefect[T](p: String): Consequence[T] = Error(Conclusion.config.illegalConfigurationDefect(p))

    def from[A](p: ParseResult[A]): Consequence[A] = p match {
      case m: ParseSuccess[_] => Success(m.ast, _conclusion_success(m))
      case m: ParseFailure[_] => Error(_conclusion_config_error(m))
      case m: EmptyParseResult[_] => Error(_conclusion_config_error(m))
    }
  }
}
