package org.goldenport.i18n

import java.nio.charset.Charset
import org.goldenport.Platform

/*
 * @since   Oct. 13, 2024
 * @version Oct. 13, 2024
 * @author  ASAMI, Tomoharu
 */
object CharsetUtils {
  def UTF8: Charset = Platform.charset.UTF8
  def WINDOWS31J: Charset = Platform.charset.WINDOWS31J
}
