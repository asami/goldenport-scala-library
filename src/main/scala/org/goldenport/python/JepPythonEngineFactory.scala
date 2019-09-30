package org.goldenport.python

import scala.collection.JavaConverters._
import javax.script._
import org.goldenport.RAISE

/*
 * @since   Sep. 19, 2019
 * @version Sep. 29, 2019
 * @author  ASAMI, Tomoharu
 */
class JepPythonEngineFactory() extends ScriptEngineFactory {
  private lazy val _engine = new JepPythonEngine(this)

  def getEngineName(): String = "JEP Python Engine"
  def getEngineVersion(): String = "0.1"
  def getLanguageName(): String = "python"
  def getLanguageVersion(): String = "2.7 or higher"
  def getNames(): java.util.List[String] = List("python", "py").asJava
  def getMimeTypes(): java.util.List[String] = List("application/x-python-code", "text/x-python").asJava
  def getExtensions(): java.util.List[String] = List("py").asJava
  def getMethodCallSyntax(obj: String, m: String, args: String*): String = RAISE.notImplementedYetDefect
  def getOutputStatement(toDisplay: String): String = RAISE.notImplementedYetDefect
  def getParameter(key: String): Object = RAISE.notImplementedYetDefect
  def getProgram(statements: String*): String = RAISE.notImplementedYetDefect
  def getScriptEngine(): ScriptEngine = _engine
}
