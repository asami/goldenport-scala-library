package org.goldenport.recorder

import java.io.File
import org.goldenport.notification.NotificationContext
import org.goldenport.observability.ObservabilityContext

/*
 * @since   Apr. 10, 2020
 *  version Jan. 24, 2021
 * @version Apr. 28, 2025
 * @author  ASAMI, Tomoharu
 */
class StandardRecorder(
  observability: ObservabilityContext,
  notification: NotificationContext
) extends RecorderBase {
  override protected def get_Observability_Context = Some(observability)

  // private var _log_drivers: Vector[LogDriver] = Vector(ConsoleLogDriver.default) // currently unused.
  // private var _message_drivers: Vector[MessageDriver] = Vector.empty
  // private var _report_drivers: Vector[ReportDriver] = Vector.empty

  def setReportFile(p: File): Unit = {
    ???
  }

  protected def out_Standard(message: String): Unit = System.out.println(message)
  protected def out_Error(message: String): Unit = System.err.println(message)
  protected def out_Report(message: String): Unit = {}
}
