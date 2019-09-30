package org.goldenport.python

import scala.collection.JavaConverters._
import java.io.Reader
import javax.script._
import jep._
import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * @since   Sep. 19, 2019
 * @version Sep. 29, 2019
 * @author  ASAMI, Tomoharu
 */
class JepPythonEngine(factory: JepPythonEngineFactory) extends AbstractScriptEngine {
//  private lazy val _interpreter = new SharedInterpreter()

  def getFactory(): ScriptEngineFactory = factory

  def createBindings(): Bindings = RAISE.notImplementedYetDefect

  def eval(reader: Reader, ctx: ScriptContext): Object = RAISE.notImplementedYetDefect

  def eval(script: String, ctx: ScriptContext): Object = {
    val lines = Strings.tolines(script)
    val interpreter = _interpreter
    lines.init.foreach(interpreter.exec)
    val r = interpreter.getValue(lines.last)
    interpreter.close()
    r
  }

  private def _interpreter = {
    val interpreter = new SharedInterpreter()
    val bindings = getBindings(ScriptContext.ENGINE_SCOPE)
    bindings.asScala.map {
      case (k, v) => interpreter.set(k, v)
    }
    interpreter
  }

  def invoke(script: String, args: Any*)(implicit ctx: ScriptContext): Object = {
    val interpreter = _interpreter
    val r = interpreter.invoke(script, args)
    interpreter.close()
    r
  }
}
