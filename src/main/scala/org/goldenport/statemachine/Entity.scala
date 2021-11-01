package org.goldenport.statemachine

import org.goldenport.event.ObjectId

/*
 * @since   Jan.  4, 2021
 * @version Oct. 31, 2021
 * @author  ASAMI, Tomoharu
 */
trait Entity[-T] {
  def id(o: T): ObjectId
}
