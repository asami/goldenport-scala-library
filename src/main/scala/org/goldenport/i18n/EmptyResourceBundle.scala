package org.goldenport.i18n

import java.util.ListResourceBundle

/*
 * @since   Sep. 25, 2019
 * @version Sep. 25, 2019
 * @author  ASAMI, Tomoharu
 */
object EmptyResourceBundle extends ListResourceBundle {
  protected def getContents(): Array[Array[AnyRef]] = Array()
}
