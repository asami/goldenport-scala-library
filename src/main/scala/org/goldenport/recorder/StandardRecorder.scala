package org.goldenport.recorder

/*
 * @since   Apr. 10, 2020
 * @version Apr. 10, 2020
 * @author  ASAMI, Tomoharu
 */
object StandardRecorder extends RecorderBase {
  protected def out_Standard(message: String): Unit = System.out.println(message)
  protected def out_Error(message: String): Unit = System.err.println(message)
  protected def out_Report(message: String): Unit = {}
}
