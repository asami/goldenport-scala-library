package org.goldenport.io

import java.io._
import java.nio.charset.Charset
import scalax.io.Codec
import org.goldenport.Platform

/*
 * @since   Jun. 24, 2019
 *  version May.  4, 2020
 * @version Feb. 26, 2022
 * @author  ASAMI, Tomoharu
 */
class StringInputStream(
  string: String,
  charset: Charset = Platform.charset.UTF8
) extends InputStream {
  import StringInputStream._

  private val _strategy = if (string.length > THRESHOLD)
    new StreamStrategy(string, charset)
  else
    new BufferStrategy(string, charset)

  def read(): Int = _strategy.read()
}

object StringInputStream {
  val THRESHOLD = 8192

  sealed trait Strategy {
    def read(): Int
  }

  class StreamStrategy(string: String, charset: Charset) extends Strategy {
    private var _index = 0
    private var _buffer: Array[Byte]  = null
    private var _buffer_index = 0

    private def _is_end = string.length <= _index && _buffer == null

    def read(): Int = if (_is_end) {
      -1
    } else {
      if (_buffer == null) {
        val nextindex = string.offsetByCodePoints(_index, 1)
        val unicode = string.substring(_index, nextindex)
//        println(unicode)
        _buffer = unicode.getBytes(charset)
//        println(_buffer.size)
        _buffer_index = 0
        _index = nextindex
      }
      val c = _buffer(_buffer_index)
//      println(c)
      if (_buffer.length <= _buffer_index + 1) {
        _buffer = null
        _buffer_index = 0
      } else {
        _buffer_index += 1
      }
//      println(_buffer_index)
      c
    }
  }

  class BufferStrategy(string: String, charset: Charset) extends Strategy {
    private val _buffer = string.getBytes(charset)
    private var _index = 0

    def read(): Int = if (_buffer.size <= _index) {
      -1
    } else {
      val c = _buffer(_index)
//      println(c)
      _index += 1
      c
    }
  }

  def apply(p: String, encoding: String): StringInputStream = new StringInputStream(p, Charset.forName(encoding))
  def apply(p: String, codec: Codec): StringInputStream = new StringInputStream(p, codec.charSet)
}
