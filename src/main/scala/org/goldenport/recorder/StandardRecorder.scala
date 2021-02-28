package org.goldenport.recorder

import java.io.File
import org.goldenport.recorder.driver._

/*
 * @since   Apr. 10, 2020
 * @version Jan. 24, 2021
 * @author  ASAMI, Tomoharu
 */
class StandardRecorder() extends RecorderBase {
  private var _log_drivers: Vector[LogDriver] = Vector(ConsoleLogDriver.default) // currently unused.
  private var _message_drivers: Vector[MessageDriver] = Vector.empty
  private var _report_drivers: Vector[ReportDriver] = Vector.empty

  def setReportFile(p: File): Unit = {
    ???
  }

  protected def out_Standard(message: String): Unit = System.out.println(message)
  protected def out_Error(message: String): Unit = System.err.println(message)
  protected def out_Report(message: String): Unit = {}
}
