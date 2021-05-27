package org.goldenport.context

import org.goldenport.exception.GoldenportException

/*
 * @since   May. 27, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
class FaultException(fault: Fault) extends GoldenportException(fault.message.en) {
}
