package org.goldenport.util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * @since   Sep.  9, 2010
 * @version Sep. 21, 2010
 * @author  ASAMI, Tomoharu
 */
// XXX com.asamioffice.goldenport.util.ArrayMap.java
class ArrayMap[A, B]() extends mutable.Map[A, B] {
  private val buffer = new ArrayBuffer[(A, B)]

  // Map
  def get(key: A): Option[B] = {
    buffer.find(kv => kv._1 == key).map(_._2)
  }

  def iterator: Iterator[(A, B)] = {
    buffer.iterator
  }

  override def +=(kv: (A, B)): this.type = {
    val idx = buffer.indexWhere(_._1 == kv._1)
    if (idx != -1) {
      buffer(idx) = kv
    } else {
      buffer += kv
    }
    this
  }

  override def -=(key: A): this.type = {
    val idx = buffer.indexWhere(_._1 == key)
    if (idx != -1) {
      buffer.remove(idx)
    }
    this
  }

  override def empty: ArrayMap[A, B] = {
    new ArrayMap[A, B]()
  }

  override def foreach[C](f: ((A, B)) => C) {
    for (kv <- buffer) f(kv)
  }

  override def size = {
    buffer.size
  }
}
