package org.goldenport.log

import java.util.logging.{Handler, LogRecord}
import org.slf4j.{Logger, LoggerFactory}

/*
 * @since   May. 10, 2025
 * @version May. 10, 2025
 * @author  ASAMI, Tomoharu
 */
class Slf4jLogHandler(name: String) extends Handler {
  private val logger: Logger = LoggerFactory.getLogger(name)

  override def publish(record: LogRecord): Unit = {
    if (!isLoggable(record)) return

    val msg = getFormatter match {
      case null => record.getMessage
      case fmt => fmt.format(record)
    }

    record.getLevel.getName match {
      case "SEVERE"  => logger.error(msg)
      case "WARNING" => logger.warn(msg)
      case "INFO"    => logger.info(msg)
      case "CONFIG"  => logger.debug(msg)
      case "FINE"    => logger.debug(msg)
      case _         => logger.trace(msg)
    }
  }

  override def flush(): Unit = ()

  override def close(): Unit = ()
}

