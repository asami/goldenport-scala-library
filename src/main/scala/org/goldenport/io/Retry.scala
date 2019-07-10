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
import org.goldenport.collection.NonEmptyVector
import org.goldenport.incident._

/*
 * @since   Dec. 10, 2014
 * @version Jun. 10, 2019
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

  def incident: RetrySequenceIncident = {
    val xs = log.toIndexedSeq.zipWithIndex.map {
      case (x, i) => RetryIncident(x.start, x.end, x.reason.map(_.message), i, x.outcome)
    }
    RetrySequenceIncident(NonEmptyVector(xs))
  }

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
      val start = System.currentTimeMillis
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
          _log_success(start, m)
          SuccessOutcome(m.v)
        case m: ValueErrorConsequence[T] =>
          _log_error(start, m)
          ValueErrorOutcome(m.v)
        case m: ValueFailureConsequence[T] =>
          if (maxTryCount > count) {
            _log_retry(start, m)
            Thread.sleep(duration)
            go(count + 1, (duration * backoffFactor).toLong)
          } else {
            _log_error(start, m)
            ValueErrorOutcome(m.v)
          }
        case m: ExceptionErrorConsequence[T] =>
          _log_error(start, m)
          ExceptionErrorOutcome(m.e)
        case m: ExceptionFailureConsequence[T] =>
          if (maxTryCount > count) {
            _log_retry(start, m)
            Thread.sleep(duration)
            go(count + 1, (duration * backoffFactor).toLong)
          } else {
            _log_error(start, m)
            ExceptionErrorOutcome(m.e)
          }
      }
    }
    go(1, initDuration.toMillis)
  }

  private def _log_success(start: Long, p: SuccessConsequence[T]): Unit =
    log += Log(SuccessEventKind, start, p.v)

  private def _log_error(start: Long, p: ValueErrorConsequence[T]): Unit =
    log += Log(ErrorEventKind, start, p.reason)

  private def _log_error(start: Long, p: ValueFailureConsequence[T]): Unit =
    log += Log(ErrorEventKind, start, p.reason)

  private def _log_error(start: Long, p: ExceptionErrorConsequence[T]): Unit =
    log += Log(ErrorEventKind, start, p.reason, p.e)

  private def _log_error(start: Long, p: ExceptionFailureConsequence[T]): Unit =
    log += Log(ErrorEventKind, start, p.reason, p.e)

  private def _log_retry(start: Long, p: ValueFailureConsequence[T]): Unit =
    log += Log(RetryEventKind, start, p.reason)

  private def _log_retry(start: Long, p: ExceptionFailureConsequence[T]): Unit =
    log += Log(RetryEventKind, start, p.reason, p.e)
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
  object ErrorStrategy {
    def apply(msg: String): ErrorStrategy = ErrorStrategy(Reason(msg))
    def apply(e: Thread): ErrorStrategy = ErrorStrategy(s"${e}")
  }
  case class RetryStrategy(reason: Reason) extends Strategy {
  }
  object RetryStrategy {
    def apply(msg: String): RetryStrategy = RetryStrategy(Reason(msg))
  }

  sealed trait Consequence[T]
  case class SuccessConsequence[T](v: T) extends Consequence[T]
  case class ValueErrorConsequence[T](v: T, reason: Reason) extends Consequence[T]
  case class ValueFailureConsequence[T](v: T, reason: Reason) extends Consequence[T]
  case class ExceptionErrorConsequence[T](e: Throwable, reason: Reason) extends Consequence[T]
  case class ExceptionFailureConsequence[T](e: Throwable, reason: Reason) extends Consequence[T]

  sealed trait Outcome[T] {
    def isSuccess: Boolean
  }
  case class SuccessOutcome[T](v: T) extends Outcome[T] {
    def isSuccess: Boolean = true
  }
  sealed trait ErrorOutcome[T] extends Outcome[T] {
    def isSuccess: Boolean = false
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

  case class Log(
    kind: EventKind,
    start: Long,
    end: Long,
    reason: Option[Reason],
    outcome: Outcome[_]
  )
  object Log {
    def apply(kind: EventKind, start: Long, v: Any): Log = Log(kind, start, System.currentTimeMillis, None, _outcome(kind, v))
    def apply(kind: EventKind, start: Long, reason: Reason, v: Any): Log = Log(kind, start, System.currentTimeMillis, Some(reason), _outcome(kind, v))
    def apply(kind: EventKind, start: Long, reason: Reason, e: Throwable): Log = Log(kind, start, System.currentTimeMillis, Some(reason), Retry.ExceptionErrorOutcome(e))

    private def _outcome(kind: EventKind, v: Any): Retry.Outcome[_] = kind match {
      case SuccessEventKind => Retry.SuccessOutcome(v)
      case ErrorEventKind => Retry.ValueErrorOutcome(v)
      case RetryEventKind => Retry.ValueErrorOutcome(v)
    }
  }

  def apply[T](body: => T): Retry[T] = create(body)

  def create[T](body: => T): Retry[T] = {
    create(body, 3, 1.second, 2.0, _ => SuccessStrategy, defaultRetryException, _ => Unit)
  }

  def create[T](
    body: => T,
    maxTryCount: Int,
    initDuration: FiniteDuration,
    backoffFactor: Double,
    isRetry: T => Strategy
  ): Retry[T] = create(body, maxTryCount, initDuration, backoffFactor, isRetry, defaultRetryException, _ => Unit)

  def create[T](
    body: => T,
    maxTryCount: Int,
    initDuration: FiniteDuration,
    backoffFactor: Double,
    isRetry: T => Strategy,
    isRetryException: Throwable => Strategy,
    onRetry: Int => Unit
  ): Retry[T] = {
    new Retry(() => body, maxTryCount, initDuration, backoffFactor, isRetry, isRetryException, onRetry)
  }

  private def _retry_condition[T](isretryp: T => Boolean): T => Strategy = (x: T) => {
    if (isretryp(x))
      RetryStrategy(Reason(s"Illegal value: $x"))
    else
      SuccessStrategy
  }

  private def _retry_exception_condition(isretryp: Throwable => Boolean): Throwable => Strategy =
    (x: Throwable) => {
      if (isretryp(x))
        RetryStrategy(s"Illegal exception: $x")
      else
        ErrorStrategy(s"Illegal exception: $x")
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
    create(body, maxTryCount, initDuration, backoffFactor, (_: T) => SuccessStrategy, _retry_exception_condition(isRetryException), onRetry).run
  }

  def defaultRetryException(e: Throwable): Strategy = {
    e match {
      case x: java.io.FileNotFoundException => {
        val msg = x.getMessage
        if (msg.startsWith("http:") || msg.startsWith("https:")) {
          ErrorStrategy(s"サーバー上にファイル「$msg」がないのでリトライせずエラーとしました。")
        } else {
          ErrorStrategy(s"ファイルアクセスエラー「$msg」のためリトライせずエラーとしました。")
        }
      }
      case x: java.net.UnknownHostException => {
        ErrorStrategy("サーバー「${x.getMessage}」が見つからないのでリトライせずエラーとしました。")
      }
      case x: IllegalArgumentException => { // image format error in ImageStore
        ErrorStrategy(s"「${x.getMessage}」のためリトライせずエラーとしました。")
      }
      case x: javax.imageio.IIOException => {
        ErrorStrategy(s"画像処理エラー「${x.getMessage}」のためリトライせずエラーとしました。")
      }
      case x if _is_fatal_sql_error(x) => {
        ErrorStrategy(s"SQLで致命的なエラー「${x.getMessage}」が発生しました。このためリトライせずエラーとしました。")
      }
      case x => RetryStrategy(s"Illegal exception: $x")
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
