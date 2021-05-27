package org.goldenport.statemachine

/*
 * @since   Jan.  4, 2021
 * @version Jan.  4, 2021
 * @author  ASAMI, Tomoharu
 */
trait Entity[-T] {
  def id(o: T): ObjectId
}
