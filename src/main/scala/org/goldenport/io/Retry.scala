package org.goldenport.io

import scala.language.existentials
import scalaz.{Success => _, _}
import scalaz.concurrent.Task
import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import java.util.concurrent.ExecutorService
import org.goldenport.RAISE
import org.goldenport.i18n.I18NString

/*
 * @since   Dec. 10, 2014
 * @version Jun.  2, 2019
 * @author  ASAMI, Tomoharu
 */
class Retry[T](
  body: () => T,
  maxTryCount: Int, // 1 means no retry  
  initDuration: FiniteDuration,
  backoffFactor: Double,
  retryCondition: T => Retry.Strategy,
  retryExceptionCondition: Throwable => Retry.Strategy,
  onRetry: Int => Unit
) {
  import Retry._

  val log = mutable.ArrayBuffer[Retry.Log]()

  def run: T = {
    _run match {
      case SuccessOutcome(v) => v
      case ValueErrorOutcome(v) => v
      case ExceptionErrorOutcome(e) => throw e
    }
  }

  def runDisjunction: Throwable \/ T = _run match {
    case SuccessOutcome(v) => \/-(v)
    case ValueErrorOutcome(v) => \/-(v)
    case ExceptionErrorOutcome(e) => -\/(throw e)
  }

  def runOption: Option[T] = _run match {
    case SuccessOutcome(v) => Some(v)
    case ValueErrorOutcome(v) => Some(v)
    case ExceptionErrorOutcome(e) => None
  }

  def runEither: Either[Throwable, T] = runDisjunction.toEither

  def runTry: Try[T] = _run match {
    case SuccessOutcome(v) => Success(v)
    case ValueErrorOutcome(v) => Success(v)
    case ExceptionErrorOutcome(e) => Failure(e)
  }

  def runFuture(implicit pool: ExecutionContext): Future[T] = Future(_run) flatMap {
    case SuccessOutcome(v) => Future.successful(v)
    case ValueErrorOutcome(v) => Future.successful(v)
    case ExceptionErrorOutcome(e) => Future.failed(e)
  }

  def runTask(implicit pool: ExecutorService): Task[T] = 
    new Task(scalaz.concurrent.Future(runDisjunction)(pool))

  private def _run: Outcome[T] = {
    @annotation.tailrec
    def go(count: Int, duration: Long): Outcome[T] = {
      val rr: Consequence[T] = try {
        val r: T = body()
        retryCondition(r) match {
          case SuccessStrategy => SuccessConsequence(r)
          case ErrorStrategy(reason) => ValueErrorConsequence(r, reason)
          case RetryStrategy(reason) => ValueFailureConsequence(r, reason)
        }
      } catch {
        case NonFatal(e) => retryExceptionCondition(e) match {
          case SuccessStrategy => RAISE.noReachDefect
          case ErrorStrategy(reason) => ExceptionErrorConsequence(e, reason)
          case RetryStrategy(reason) => ExceptionFailureConsequence(e, reason)
        }
      }
      rr match {
        case m: SuccessConsequence[T] =>
          _log_success(m)
          SuccessOutcome(m.v)
        case m: ValueErrorConsequence[T] =>
          _log_error(m)
          ValueErrorOutcome(m.v)
        case m: ValueFailureConsequence[T] =>
          if (maxTryCount > count) {
            _log_retry(m)
            Thread.sleep(duration)
            go(count + 1, (duration * backoffFactor).toLong)
          } else {
            _log_error(m)
            ValueErrorOutcome(m.v)
          }
        case m: ExceptionErrorConsequence[T] =>
          _log_error(m)
          ExceptionErrorOutcome(m.e)
        case m: ExceptionFailureConsequence[T] =>
          if (maxTryCount > count) {
            _log_retry(m)
            Thread.sleep(duration)
            go(count + 1, (duration * backoffFactor).toLong)
          } else {
            _log_error(m)
            ExceptionErrorOutcome(m.e)
          }
      }
    }
    go(1, initDuration.toMillis)
  }

  private def _log_success(p: SuccessConsequence[T]): Unit =
    log += Log(SuccessEventKind)

  private def _log_error(p: ValueErrorConsequence[T]): Unit =
    log += Log(ErrorEventKind, p.reason)

  private def _log_error(p: ValueFailureConsequence[T]): Unit =
    log += Log(ErrorEventKind, p.reason)

  private def _log_error(p: ExceptionErrorConsequence[T]): Unit =
    log += Log(ErrorEventKind, p.reason)

  private def _log_error(p: ExceptionFailureConsequence[T]): Unit =
    log += Log(ErrorEventKind, p.reason)

  private def _log_retry(p: ValueFailureConsequence[T]): Unit =
    log += Log(RetryEventKind, p.reason)

  private def _log_retry(p: ExceptionFailureConsequence[T]): Unit =
    log += Log(RetryEventKind, p.reason)
}

object Retry {
  case class Reason(message: I18NString)
  object Reason {
    def apply(p: String): Reason = Reason(I18NString(p))
  }

  sealed trait Strategy {
  }
  case object SuccessStrategy extends Strategy {
  }
  case class ErrorStrategy(reason: Reason) extends Strategy {
  }
  case class RetryStrategy(reason: Reason) extends Strategy {
  }

  sealed trait Consequence[T]
  case class SuccessConsequence[T](v: T) extends Consequence[T]
  case class ValueErrorConsequence[T](v: T, reason: Reason) extends Consequence[T]
  case class ValueFailureConsequence[T](v: T, reason: Reason) extends Consequence[T]
  case class ExceptionErrorConsequence[T](e: Throwable, reason: Reason) extends Consequence[T]
  case class ExceptionFailureConsequence[T](e: Throwable, reason: Reason) extends Consequence[T]

  sealed trait Outcome[T] {
  }
  case class SuccessOutcome[T](v: T) extends Outcome[T] {
  }
  sealed trait ErrorOutcome[T] extends Outcome[T] {
  }
  case class ValueErrorOutcome[T](v: T) extends ErrorOutcome[T] {
  }
  case class ExceptionErrorOutcome[T](e: Throwable) extends ErrorOutcome[T] {
  }

  sealed trait EventKind {
  }
  case object SuccessEventKind extends EventKind {
  }
  case object ErrorEventKind extends EventKind {
  }
  case object RetryEventKind extends EventKind {
  }

  case class Log(kind: EventKind, reason: Option[Reason])
  object Log {
    def apply(kind: EventKind): Log = Log(kind, None)
    def apply(kind: EventKind, reason: Reason): Log = Log(kind, Some(reason))
  }

  def apply[T](body: => T): Retry[T] = create(body)

  def create[T](body: => T): Retry[T] = {
    create(body, 3, 1.second, 2.0, _ => true, defaultRetryException, _ => Unit)
  }

  def create[T](
    body: => T,
    maxTryCount: Int,
    initDuration: FiniteDuration,
    backoffFactor: Double,
    isRetry: T => Boolean
  ): Retry[T] = create(body, maxTryCount, initDuration, backoffFactor, isRetry, defaultRetryException, _ => Unit)

  def create[T](
    body: => T,
    maxTryCount: Int,
    initDuration: FiniteDuration,
    backoffFactor: Double,
    isRetry: T => Boolean,
    isRetryException: Throwable => Boolean,
    onRetry: Int => Unit
  ): Retry[T] = {
    new Retry(() => body, maxTryCount, initDuration, backoffFactor, _retry_condition(isRetry), _retry_exception_condition(isRetryException), onRetry)
  }

  private def _retry_condition[T](p: T => Boolean): T => Strategy = (x: T) => {
    if (p(x))
      SuccessStrategy
    else
      RetryStrategy(Reason(s"Illegal value: $x"))
  }

  private def _retry_exception_condition(p: Throwable => Boolean): Throwable => Strategy =
    (x: Throwable) => {
      if (p(x))
        SuccessStrategy
      else
        RetryStrategy(Reason(s"Illegal exception: $x"))
    }

  def retry[T](body: => T): T = {
    create(body).run
  }

  def retry[T](
    body: => T,
    maxTryCount: Int,
    initDuration: FiniteDuration,
    backoffFactor: Double,
    isRetryException: Throwable => Boolean,
    onRetry: Int => Unit
  ): T = {
    create(body, maxTryCount, initDuration, backoffFactor, (_: T) => true, isRetryException, onRetry).run
  }

  def defaultRetryException(e: Throwable): Boolean = {
    e match {
      case x: java.io.FileNotFoundException => {
        val msg = x.getMessage
        if (msg.startsWith("http:") || msg.startsWith("https:")) {
          //        log_skip(r, "サーバー上にファイル「%s」がないので「%s」をスキップしました。".format(msg, RecordUtils.formatSnapshot(r)))
          false
        } else {
          //        log_skip(r, "ファイルアクセスエラー「%s」のため「%s」をスキップしました。".format(msg, RecordUtils.formatSnapshot(r)))
          false
        }
      }
      case x: java.net.UnknownHostException => {
        //      log_skip(r, "サーバー「%s」が見つからないので「%s」をスキップしました。".format(x.getMessage, RecordUtils.formatSnapshot(r)))
        false
      }
      case x: IllegalArgumentException => { // image format error in ImageStore
                                            //      log_skip(r, "「%s」のため「%s」をスキップしました。".format(x.getMessage, RecordUtils.formatSnapshot(r)))
        false
      }
      case x: javax.imageio.IIOException => {
        //      log_skip(r, "画像処理エラー「%s」のため「%s」をスキップしました。".format(x.getMessage, RecordUtils.formatSnapshot(r)))
        false
      }
      case x if _is_fatal_sql_error(x) => {
        //log_skip(r, "SQLで致命的なエラー「%s」が発生しました。このため「%s」をスキップしました。".format(x.getMessage, RecordUtils.formatSnapshot(r)))
        false
      }
    }
  }

  private def _is_fatal_sql_error(e: Throwable) = {
    e.getClass.getName match {
      case "com.mysql.jdbc.exceptions.jdbc4.MySQLIntegrityConstraintViolationException" => true
      case "com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException" => true
      case "com.mysql.jdbc.MysqlDataTruncation" => true
      case _ => false
    }
  }
}
