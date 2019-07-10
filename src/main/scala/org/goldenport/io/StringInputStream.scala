package org.goldenport.io

import java.io._
import java.nio.charset.Charset
import org.goldenport.Platform

/*
 * @since   Jun. 24, 2019
 * @version Jun. 24, 2019
 * @author  ASAMI, Tomoharu
 */
class StringInputStream(
  string: String,
  charset: Charset = Platform.charset.UTF8
) extends InputStream {
  import StringInputStream._

  private val _strategy = if (string.length > 8192)
    new StreamStrategy(string, charset)
  else
    new BufferStrategy(string, charset)

  def read(): Int = _strategy.read()
}

object StringInputStream {
  sealed trait Strategy {
    def read(): Int
  }

  class StreamStrategy(string: String, charset: Charset) extends Strategy {
    private var _index = 0
    private var _buffer: Array[Byte]  = null
    private var _buffer_index = 0

    def read(): Int = if (string.length < _index) {
      -1
    } else {
      if (_buffer == null) {
        val unicode = string.substring(_index, _index + 1)
        _buffer = unicode.getBytes(charset)
        _buffer_index = 0
      }
      val c = _buffer(_buffer_index)
      if (_buffer.length <= _buffer_index + 1) {
        _buffer = null
        _buffer_index = 0
      }
      c
    }
  }

  class BufferStrategy(string: String, charset: Charset) extends Strategy {
    private val _buffer = string.getBytes(charset)
    private var _index = 0

    def read(): Int = if (_buffer.size < _index) {
      -1
    } else {
      val c = _buffer(_index)
      _index += 1
      c
    }
  }
}
