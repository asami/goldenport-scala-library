package org.goldenport.context

import scalaz._, Scalaz._
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import scala.util.control.NonFatal
import java.util.Locale
import org.goldenport.i18n.I18NString
import org.goldenport.i18n.I18NMessage
import org.goldenport.parser.{ParseResult, ParseSuccess, ParseFailure, EmptyParseResult}
import org.goldenport.parser.{ParseMessage}
import org.goldenport.cli.spec
import org.goldenport.util.AnyUtils
import org.goldenport.extension.IRecord

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
 *  version Mar. 10, 2022
 *  version Apr.  3, 2022
 *  version May. 31, 2022
 *  version Jun. 14, 2022
 *  version Jul. 27, 2022
 *  version Aug.  3, 2022
 *  version Sep.  3, 2022
 *  version Oct. 31, 2022
 *  version Nov. 27, 2022
 *  version Dec. 31, 2022
 *  version Jan. 25, 2023
 *  version Jul. 23, 2023
 *  version Sep. 27, 2023
 *  version Mar.  9, 2025
 * @version Apr. 21, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait Consequence[+T] {
  def conclusion: Conclusion
  def code: Int = conclusion.code
  def toOption: Option[T]
  def toTry: Try[T]
  def some: Consequence[Option[T]]
  def none: Consequence[Option[T]]
  def unit: Consequence[Unit]
  def asError[U]: Consequence[U]
  def add(p: Conclusion): Consequence[T]
  def map[U](f: T => U): Consequence[U]
  // Consequence is not Monad. Just to use 'for' comprehension in Scala syntax suger.
  def flatMap[U](f: T => Consequence[U]): Consequence[U]
  def foreach[U](f: T => U): Unit
  def flatten[U](implicit ev: <:<[T, Consequence[U]]): Consequence[U]
  def transform[U](s: T => Consequence[U], f: Conclusion => Consequence[U]): Consequence[U]
  def mapConclusion(f: Conclusion => Conclusion): Consequence[T]
  def forConfig: Consequence[T]

  def toPayload(): Consequence.Payload
  def toPayload(f: T => Any): Consequence.Payload
  def toPayload(f: (Conclusion => Conclusion.Payload, T => Any)): Consequence.Payload

  // def getMessage: Option[String] = conclusion.getMessage
  def message: String = conclusion.message
  // def getMessage(locale: Locale): Option[String] = conclusion.getMessage(locale)
  def message(locale: Locale): String = conclusion.message(locale)

  def isSuccess: Boolean
  def isError: Boolean = !isSuccess
  def hasWarning: Boolean = isSuccess && !conclusion.isSuccess

  def get: Option[T] = toOption
  def getOrElse[TT >: T](e: => TT): TT = get getOrElse e
  def fold[U](fa: Conclusion => U, fb: T => U): U
  def foldConclusion[U >: T](f: Conclusion => U): U

  def orElse[TT >: T](p: => Consequence[TT]): Consequence[TT]

  def onSuccess[TT >: T](p: T => TT): Consequence[TT]
  def onError[TT >: T](p: Conclusion => Consequence[TT]): Consequence[TT]
  def recover[U >: T](pf: PartialFunction[Conclusion, U]): Consequence[U]
  def recoverWith[U >: T](pf: PartialFunction[Conclusion, Consequence[U]]): Consequence[U]

  def take: T

  def getException: Option[Throwable]

  def takeOrInvalidArgumentFault(message: String): T = get getOrElse Conclusion.invalidArgumentFault(message).RAISE

  def takeOrIllegalConfigurationDefect(message: String): T = get getOrElse Conclusion.config.illegalConfigurationDefect(message).RAISE
}

object Consequence {
  case class Success[+T](
    result: T,
    conclusion: Conclusion = Conclusion.Ok
  ) extends Consequence[T] {
    def isSuccess: Boolean = true
    def toOption: Option[T] = Some(result)
    def toTry: Try[T] = Try(result)
    def some: Consequence[Option[T]] = copy(result = Some(result))
    def none: Consequence[Option[T]] = copy(result = None)
    def unit: Consequence[Unit] = copy(result = Unit)
    def asError[U]: Consequence[U] = Consequence.noReachDefect("Force error in as operation.")
    def getException: Option[Throwable] = None
    def add(p: Conclusion): Consequence[T] = copy(conclusion = conclusion + p)
    def map[U](f: T => U): Consequence[U] = copy(result = f(result))
    def flatMap[U](f: T => Consequence[U]): Consequence[U] = f(result).add(conclusion)
    def foreach[U](f: T => U): Unit = f(result)
    def flatten[U](implicit ev: <:<[T, Consequence[U]]): Consequence[U] = result
    def transform[U](s: T => Consequence[U], f: Conclusion => Consequence[U]): Consequence[U] = flatMap(s)
    def mapConclusion(f: Conclusion => Conclusion): Consequence[T] = copy(conclusion = f(conclusion))
    def fold[U](fa: Conclusion => U, fb: T => U): U = fb(result)
    def foldConclusion[U >: T](f: Conclusion => U): U = result.asInstanceOf[U]
    def orElse[TT >: T](p: => Consequence[TT]): Consequence[TT] = this
    def onSuccess[TT >: T](p: T => TT): Consequence[TT] = map(p)
    def onError[TT >: T](p: Conclusion => Consequence[TT]): Consequence[TT] = this
    def recover[U >: T](pf: PartialFunction[Conclusion, U]): Consequence[U] = this
    def recoverWith[U >: T](pf: PartialFunction[Conclusion, Consequence[U]]): Consequence[U] = this
    def take = result
    def forConfig: Consequence[T] = if (conclusion.isSuccess) this else copy(conclusion = conclusion.forConfig)

    def toPayload() = Payload(conclusion.toPayload, Some(result), Map.empty)
    def toPayload(f: T => Any) = Payload(conclusion.toPayload, Some(f(result)), Map.empty)
    def toPayload(f: (Conclusion => Conclusion.Payload, T => Any)): Consequence.Payload =
      Payload(f._1(conclusion), Some(f._2(result)), Map.empty)
  }

  case class Error[+T](
    conclusion: Conclusion = Conclusion.InternalServerError
  ) extends Consequence[T] {
    def isSuccess: Boolean = false
    def toOption: Option[T] = None
    def toTry: Try[T] = TryFailure(conclusion.toException)
    def some: Consequence[Option[T]] = this.asInstanceOf[Consequence[Option[T]]]
    def none: Consequence[Option[T]] = this.asInstanceOf[Consequence[Option[T]]]
    def unit: Consequence[Unit] = this.asInstanceOf[Consequence[Unit]]
    def asError[U]: Consequence[U] = this.asInstanceOf[Consequence[U]]
    def getException: Option[Throwable] = Some(conclusion.toException)
    def add(p: Conclusion): Consequence[T] = copy(conclusion = conclusion + p)
    def map[U](f: T => U): Consequence[U] = this.asInstanceOf[Error[U]]
    def flatMap[U](f: T => Consequence[U]): Consequence[U] = this.asInstanceOf[Consequence[U]]
    def foreach[U](f: T => U): Unit = {}
    def mapConclusion(f: Conclusion => Conclusion): Consequence[T] = copy(conclusion = f(conclusion))
    def flatten[U](implicit ev: <:<[T, Consequence[U]]): Consequence[U] = this.asInstanceOf[Consequence[U]]
    def transform[U](s: T => Consequence[U], f: Conclusion => Consequence[U]): Consequence[U] =
      f(conclusion)
    def fold[U](fa: Conclusion => U, fb: T => U): U = fa(conclusion)
    def foldConclusion[U >: T](f: Conclusion => U): U = f(conclusion)
    def orElse[TT >: T](p: => Consequence[TT]): Consequence[TT] = p
    def onSuccess[TT >: T](p: T => TT): Consequence[TT] = this
    def onError[TT >: T](p: Conclusion => Consequence[TT]): Consequence[TT] = p(conclusion)
    def recover[U >: T](pf: PartialFunction[Conclusion, U]): Consequence[U] =
      Consequence.run(
        if (pf isDefinedAt conclusion)
          Consequence(pf(conclusion))
        else
          this
      )
    def recoverWith[U >: T](pf: PartialFunction[Conclusion, Consequence[U]]): Consequence[U] =
      Consequence.run(
        if (pf isDefinedAt conclusion)
          pf(conclusion)
        else
          this
      )

    def withException(e: Throwable): Error[T] = copy(conclusion = conclusion.withException(e))

    def take = RAISE
    def forConfig: Consequence[T] = if (conclusion.isSuccess) this else copy(conclusion = conclusion.forConfig)

    def toPayload() = Payload(conclusion.toPayload, None, Map.empty)
    def toPayload(f: T => Any) = Payload(conclusion.toPayload, None, Map.empty)
    def toPayload(f: (Conclusion => Conclusion.Payload, T => Any)): Consequence.Payload =
      Payload(f._1(conclusion), None, Map.empty)

    def RAISE: Nothing = throw getException.getOrElse(new ConsequenceException(this))
    def RAISEC: Nothing = throw new ConsequenceException(this)
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
  def some[T, U](p: U)(implicit ev: <:<[T, Option[U]]): Consequence[T] = Success(Some(p).asInstanceOf[T])
  def none[T, U](implicit ev: <:<[T, Option[U]]): Consequence[T] = Success(None.asInstanceOf[T])
  def successStatus[T](p: T, s: Int): Consequence[T] = Success(p, Conclusion(s))
  def successStatus[T](p: T, s: StatusCode): Consequence[T] = Success(p, Conclusion(s))
  def warning[T](p: T, c: Conclusion): Consequence[T] = Success(p, c)
  def http[T](code: Int, p: T): Consequence[T] = {
    val s = StatusCode(code)
    if (s.isSuccess)
      successStatus(p, s)
    else
      errorHttpBody(s, p)
  }

  private val _unit = Consequence.success(Unit)
  def unit[T] = _unit.asInstanceOf[Consequence[T]]
  private val _none = Consequence.success(None)
  def none[T] = _none.asInstanceOf[Consequence[T]]

  // Generic error derived from HTTP
  def badRequest[T](p: String): Consequence[T] = badRequest(I18NString(p))
  def badRequest[T](p: I18NString): Consequence[T] = error(400, p)
  def badRequest[T](p: Throwable): Consequence[T] = error(400, p)
  def unauthorized[T](p: String): Consequence[T] = unauthorized(I18NString(p))
  def unauthorized[T](en: String, ja: String): Consequence[T] = unauthorized(I18NString(en, ja))
  def unauthorized[T](p: I18NString): Consequence[T] = error(401, p)
  def unauthorized[T](p: Throwable): Consequence[T] = error(401, p)
  def paymentRequired[T](p: String): Consequence[T] = paymentRequired(I18NString(p))
  def paymentRequired[T](p: I18NString): Consequence[T] = error(402, p)
  def forbidden[T](p: String): Consequence[T] = forbidden(I18NString(p))
  def forbidden[T](p: I18NString): Consequence[T] = error(403, p)
  def notFound[T](p: String): Consequence[T] = notFound(I18NString(p))
  def notFound[T](p: I18NString): Consequence[T] = error(404, p)
  def notFound[T](p: Throwable): Consequence[T] = error(404, p)
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
  def internalServerError[T](p: Throwable): Consequence[T] = error(500, p)
  def unsupportedOperation[T](p: String): Consequence[T] = Error(Conclusion.unsupportedOperationFault(p))
  def notImplemented[T](p: String): Consequence[T] = notImplemented(I18NString(p))
  def notImplemented[T](p: I18NString): Consequence[T] = error(501, p)
  def notImplemented[T](p: Throwable): Consequence[T] = error(501, p)
  def badGateway[T](p: String): Consequence[T] = badGateway(I18NString(p))
  def badGateway[T](p: I18NString): Consequence[T] = error(502, p)
  def serviceUnavailable[T](p: String): Consequence[T] = serviceUnavailable(I18NString(p))
  def serviceUnavailable[T](p: I18NString): Consequence[T] = error(503, p)
  def gatewayTimeout[T](p: String): Consequence[T] = gatewayTimeout(I18NString(p))
  def gatewayTimeout[T](p: I18NString): Consequence[T] = error(504, p)

  //
  def error[T](c: Conclusion): Consequence[T] = Error(c)
  def error[T](c: Conclusion, message: String): Consequence[T] = Error(c.withMessage(message))
  def error[T](code: Int, p: String): Consequence[T] = error(code, I18NString(p))
  def error[T](code: Int, p: I18NString): Consequence[T] = Error(Conclusion.error(code, p))
  def error[T](code: Int, e: Throwable): Consequence[T] = Error(Conclusion.error(code, e))
  def error[T](code: Int, msg: String, e: Throwable): Consequence[T] = error(code, I18NString(msg), e)
  def error[T](code: Int, msg: I18NString, e: Throwable): Consequence[T] = Error(Conclusion.error(code, msg, e))
  def error[T](code: Int, msg: I18NMessage, e: Throwable): Consequence[T] = Error(Conclusion.error(code, msg, e))
  def error[T](e: Throwable): Consequence[T] = e match {
    case m: IllegalArgumentException => badRequest(m)
    case m: SecurityException => unauthorized(m)
    case m: UnsupportedOperationException => notImplemented(m)
    case m: NoSuchElementException => notFound(m)
    case m: java.io.FileNotFoundException => notFound(m)
    case m => internalServerError(m)
  }
  def errorData[T](code: Int, p: Conclusion.ExceptionData): Consequence[T] = Error(Conclusion.errorData(code, p))
  def errorData[T](s: StatusCode, p: Conclusion.ExceptionData): Consequence[T] = Error(Conclusion.errorData(s, p))
  def errorData[T](c: Conclusion, p: Conclusion.ExceptionData): Consequence[T] = Error(c.withExceptionData(p))
  def errorData[T](c: Conclusion, p: IRecord): Consequence[T] = Error(c.withDataRecord(p))
  def errorHttpBody[T](code: Int, p: Any): Consequence[T] = errorHttpBody(Conclusion(code), p)
  def errorHttpBody[T](s: StatusCode, p: Any): Consequence[T] = errorHttpBody(Conclusion(s), p)
  def errorHttpBody[T](c: Conclusion, p: Any): Consequence[T] = errorData(c, Conclusion.ExceptionData.HttpBody(p))

  //
  def successOrMissingPropertyFault[T](name: String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(missingPropertyFault(name))
  // def successOrMissingPropertyOrError[T](name: String, p: Option[Left[String, T]]): Consequence[T] =
  //   p.map(success).getOrElse(missingPropertyOrError(name))

  // Specific error with detail code.

  def successOrMissingArgumentFault[T](key: String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(missingArgumentFault(key))

  def successOrInvalidArgumentFault[T](key: String, value: Any, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(invalidArgumentFault(key, value))

  def successOrInvalidTokenFault[T](name: String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(invalidTokenFault(name))

  def successOrBadRequestFault[T](message: => String, p: Option[T]): Consequence[T] =
    p.map(success).getOrElse(badRequest(message))

  def successOrBadRequestFault[T](p: Option[T])(e: => Throwable): Consequence[T] =
    p.map(success).getOrElse(badRequest(e))

  def takeOrNoReachDefect[T](message: String, p: => Option[T]): Consequence[T] =
    Consequence.execute(p).flatMap {
      case Some(s) => success(s)
      case None => noReachDefect(message)
    }

  def orNoReachDefect[T](p: Option[T], message: String): Consequence[T] = p.map(success) getOrElse noReachDefect(message)

  def invalidArgumentFault[T](key: String, value: Any): Consequence[T] = invalidArgumentFault(s"$key: ${AnyUtils.toShow(value)}")

  def invalidArgumentFault[T](p: String): Consequence[T] = invalidArgumentFault(I18NMessage(p))
  def invalidArgumentFault[T](p: String, arg: Any, args: Any*): Consequence[T] = Error(Conclusion.invalidArgumentFault(I18NMessage(p, args +: args)))
  def invalidArgumentFault[T](p: I18NMessage): Consequence[T] = Error(Conclusion.invalidArgumentFault(p))

  def missingArgumentFault[T](p: String, ps: String*): Consequence[T] = missingArgumentFault(p +: ps)
  def missingArgumentFault[T](ps: Seq[String]): Consequence[T] = Error(Conclusion.missingArgumentFault(ps))

  def tooManyArgumentsFault[T](p: Any, ps: Any*): Consequence[T] = tooManyArgumentsFault(p +: ps)
  def tooManyArgumentsFault[T](ps: Seq[Any]): Consequence[T] = Error(Conclusion.tooManyArgumentsFault(ps))

  def invalidPropertyFault[T](p: String): Consequence[T] = invalidPropertyFault(I18NMessage(p))
  def invalidPropertyFault[T](p: String, arg: Any, args: Any*): Consequence[T] = Error(Conclusion.invalidPropertyFault(I18NMessage(p, args +: args)))
  def invalidPropertyFault[T](p: I18NMessage): Consequence[T] = Error(Conclusion.invalidPropertyFault(p))

  def missingPropertyFault[T](p: String, ps: String*): Consequence[T] = missingPropertyFault(p +: ps)
  def missingPropertyFault[T](ps: Seq[String]): Consequence[T] = Error(Conclusion.missingPropertyFault(ps))

  def invalidTokenFault[T](value: String): Consequence[T] = Error(Conclusion.invalidTokenFault(value))
  def invalidTokenFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.invalidTokenFault(label, value))

  def valueDomainFault[T](value: String): Consequence[T] = Error(Conclusion.valueDomainFault(value))
  def valueDomainFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.valueDomainFault(label, value))
  def valueDomainFault[T](name: String, datatype: spec.DataType, value: Any): Consequence[T] = Error(
    Conclusion.valueDomainFault(name, datatype, value)
  )

  def syntaxErrorFault[T](message: String): Consequence[T] = Error(Conclusion.syntaxErrorFault(message))

  def syntaxErrorFault[T](messages: Seq[Message]): Consequence[T] = Error(Conclusion.syntaxErrorFault(messages))

  def formatErrorFault[T](message: String): Consequence[T] = Error(Conclusion.formatErrorFault(message))

  def formatErrorFault[T](messages: Seq[Message]): Consequence[T] = Error(Conclusion.formatErrorFault(messages))

  def unmarshallingDefect[T](p: String): Consequence[T] = Error(Conclusion.unmarshallingDefect(p))

  def databaseIoFault[T](message: String): Consequence[T] = Error(Conclusion.databaseIoFault(message))

  def noReachDefect[T](message: String): Consequence[T] = Error(Conclusion.noReachDefect(message))

  //
  def execute[T](body: => T): Consequence[T] = try {
    Success(body)
  } catch {
    case NonFatal(e) => error(e)
  }

  def executeOption[T](p: => Option[T])(c: => Conclusion): Consequence[T] =
    for {
      x <- Consequence(p)
      r <- x match {
        case Some(s) => Consequence.success(s)
        case None => Consequence.error(c)
      }
    } yield r

  def getOrExecute[T](p: Option[T])(x: => T): Consequence[T] =
    p.map(success).getOrElse(Consequence(x))

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

  def getOrRun[T](p: Option[T], q: => Consequence[T]): Consequence[T] =
    p.map(success) getOrElse run(q)

  def orRun[T](p: Option[T], q: => Consequence[Option[T]]): Consequence[Option[T]] =
    p match {
      case Some(s) => Consequence.success(Some(s))
      case None => run(q)
    }

  def runOrMissingPropertyFault[T](name: String)(p: => Consequence[Option[T]]): Consequence[T] =
    run(p).flatMap {
      case Some(s) => Success(s)
      case None => Error(Conclusion.missingPropertyFault(name))
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

  def from[A](p: Try[A]): Consequence[A] = p match {
    case TrySuccess(s) => Consequence.success(s)
    case TryFailure(e) => error(e)
  }

  def from[E <: Throwable, A](p: Either[E, A]): Consequence[A] = p match {
    case Right(r) => Consequence.success(r)
    case Left(e) => error(e)
  }

  object config {
    def successOrMissingPropertyFault[T](name: String, p: Option[T]): Consequence[T] =
      p.map(success).getOrElse(missingPropertyFault(name))
  // def successOrMissingPropertyOrError[T](name: String, p: Option[Left[String, T]]): Consequence[T] =
  //   p.map(success).getOrElse(missingPropertyOrError(name))

  // Specific error with detail code.

    def successOrInvalidPropertyFault[T](name: String, value: String, p: Option[T]): Consequence[T] =
      p.map(success).getOrElse(invalidPropertyFault(name, value))

    def successOrInvalidTokenFault[T](name: String, p: Option[T]): Consequence[T] =
      p.map(success).getOrElse(invalidTokenFault(name))

    def invalidPropertyFault[T](p: String): Consequence[T] = Error(Conclusion.config.invalidPropertyFault(p))

    def invalidPropertyFault[T](key: String, value: Any): Consequence[T] = Error(Conclusion.config.invalidPropertyFault(key, value))

    def missingPropertyFault[T](p: String): Consequence[T] = Error(Conclusion.config.missingPropertyFault(p))

    def invalidTokenFault[T](value: String): Consequence[T] = Error(Conclusion.config.invalidTokenFault(value))

    def invalidTokenFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.config.invalidTokenFault(label, value))

    def valueDomainFault[T](value: String): Consequence[T] = Error(Conclusion.config.valueDomainFault(value))

    def valueDomainFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.config.valueDomainFault(label, value))

    def illegalConfigurationDefect[T](p: String): Consequence[T] = Error(Conclusion.config.illegalConfigurationDefect(p))

    def capacityOverflowFault[T](label: String, value: String): Consequence[T] = Error(Conclusion.config.capacityOverflowFault(label, value))

    def from[A](p: ParseResult[A]): Consequence[A] = p match {
      case m: ParseSuccess[_] => Success(m.ast, _conclusion_success(m))
      case m: ParseFailure[_] => Error(_conclusion_config_error(m))
      case m: EmptyParseResult[_] => Error(_conclusion_config_error(m))
    }
  }

  @SerialVersionUID(1L)
  case class Payload(
    conclusion: Conclusion.Payload,
    content: Option[Any],
    properties: Map[String, String]
  ) {
    // def reconstitute[T](p: Payload => Consequence[T]): Consequence[T] = {
    //   val c = conclusion.reconstitute()
    //   if (c.isSuccess) {
    //     for {
    //       a <- p(this)
    //       r <- Consequence.Success(a, c)
    //     } yield r
    //   } else {
    //       Consequence.Error(c)
    //     }
    // }

    def reconstitute[T](p: PartialFunction[Any, Consequence[T]]): Consequence[T] = {
      val c = conclusion.reconstitute()
      if (c.isSuccess) {
        content match {
          case Some(s) => p.lift(s) match {
            case Some(ss) => ss
            case None => Consequence.unmarshallingDefect(AnyUtils.toString(s))
          }
          case None => Consequence.unmarshallingDefect(AnyUtils.toString("No content"))
        }
      } else {
        Consequence.Error(c)
      }
    }

    def mapConclusion(f: Conclusion.Payload => Conclusion.Payload): Payload =
      copy(conclusion = f(conclusion))
  }
}
