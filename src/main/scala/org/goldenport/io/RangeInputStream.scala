package org.goldenport.io

import java.io.InputStream

/*
 * @since   Oct.  5, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
class RangeInputStream(
  in: InputStream,
  start: Long,
  end: Option[Long] // exclude
) extends InputStream {
  require (end.map(_ >= start) getOrElse true,
    s"the start($start) should be less equal than the end($end)")
  in.skip(start)

  private var _current_offset = start

  private def _is_finished = {
    end match {
      case Some(s) => s <= _current_offset
      case None => false
    }
  }

  def read(): Int = {
    if (_is_finished)
      -1
    else {
      _current_offset += 1
      in.read()
    }
  }

  override def read(buf: Array[Byte]): Int = read(buf, 0, buf.length)

  override def read(buf: Array[Byte], off: Int, len: Int): Int = {
    if (_is_finished)
      -1
    else {
      val l: Int = end match {
        case Some(s) =>
          val a = (_current_offset + len) - s
          if (a < 0)
            len
          else
            (len - a).toInt
        case None => len
      }
      _current_offset += l
      in.read(buf, off, l)
    }
  }

  override def skip(n: Long): Long = in.skip(n)
  override def available(): Int = in.available()
  override def close(): Unit = in.close()
  override def mark(readlimit: Int): Unit = in.mark(readlimit)
  override def reset(): Unit = in.reset()
  override def markSupported(): Boolean = in.markSupported()
}
