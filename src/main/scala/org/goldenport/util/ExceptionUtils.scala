package org.goldenport.util

import java.lang.reflect.InvocationTargetException
import java.nio.charset._

/*
 * @since   Feb. 21, 2021
 * @version Feb. 21, 2021
 * @author  ASAMI, Tomoharu
 */
object ExceptionUtils {
  def toMessage(e: Throwable): String = {
    val a = if (e.getClass.getName == "java.lang.RuntimeException")
      Option(e.getCause)
    else
      None
    a.fold(to_message(e))(to_message)
  }

  protected def to_message(e: Throwable): String = {
    val name = e.getClass.getName
    val showname = if (name.startsWith("java.") || name.startsWith("scala."))
      s"${e.getClass.getSimpleName}"
    else
      name
    val msg = e match {
      case m: MalformedInputException => Some(s"Malformed input (e.g. encoding): ${m.getMessage}")
      case m: UnmappableCharacterException => Some(s"Unmappable character: ${m.getMessage}")
      case m: CharacterCodingException => Some(s"Character coding problem: ${m.getMessage}")
      case m => Option(e.getMessage)
    }
    msg.map(x => s"[$showname]$x").getOrElse(showname)
  }

  def toMessageShallow(e: Throwable): String = {
    val a = if (e.getClass.getName == "java.lang.RuntimeException")
      Option(e.getCause)
    else
      None
    a.fold(to_message_shallow(e))(to_message)
  }

  protected def to_message_shallow(e: Throwable): String = {
    val name = e.getClass.getName
    val showname = if (name.startsWith("java.") || name.startsWith("scala."))
      s"${e.getClass.getSimpleName}"
    else
      name
    showname
  }

  def normalize(p: Throwable): Throwable = p match {
    case m: InvocationTargetException => Option(m.getTargetException) orElse Option(m.getCause) getOrElse m
    case m => m
  }
}
