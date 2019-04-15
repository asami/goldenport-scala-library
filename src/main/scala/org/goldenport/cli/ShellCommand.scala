package org.goldenport.cli

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import java.io.{File, InputStream}
import org.goldenport.Strings
import org.goldenport.io.{InputSource, StringInputSource}
import org.goldenport.bag.{ChunkBag, BufferFileBag}

/*
 * @since   Oct.  4, 2018
 * @version Oct.  8, 2018
 * @author  ASAMI, Tomoharu
 */
class ShellCommand(
  commands: Seq[String],
  environmentVariables: Map[String, String],
  directory: Option[File],
  in: Option[InputSource],
  timeout: Option[Duration]
) {
  private val _commands = {
    val a = Vector("sh", "-c") // TODO
    val b = a :+ commands.mkString(" ")
    b.asJava
  }

  def run: ShellCommand.Result = {
    val pb = new ProcessBuilder(_commands)
    val env = pb.environment()
    environmentVariables.foreach {
      case (k, v) => env.put(k, v)
    }
    directory.foreach(pb.directory)
    val p = pb.start
    in.foreach { x =>
      Future(x.writeClose(p.getOutputStream()))
    }
    val stdout = Future {
      val bag = new BufferFileBag()
      bag.writeClose(p.getInputStream())
      bag
    }
    val stderr = Future {
      val bag = new BufferFileBag()
      bag.writeClose(p.getErrorStream())
      bag
    }
    new ShellCommand.Result(p, stdout, stderr, timeout)
  }
}

object ShellCommand {
  class Result(
    process: Process,
    stdoutFuture: Future[ChunkBag],
    stderrFuture: Future[ChunkBag],
    timeout: Option[Duration]
  ) {
    def waitFor: Int = process.waitFor
    def stdout: ChunkBag = Await.result(stdoutFuture, Duration.Inf)
    def stderr: ChunkBag = Await.result(stderrFuture, Duration.Inf)
  }

  def create(cmds: String): ShellCommand = new ShellCommand(
    Strings.totokens(cmds, " "),
    Map.empty,
    None,
    None,
    None
  )

  def run(cmds: String): Boolean = create(cmds).run.waitFor == 0

  // def main(args: Array[String]) {
  //   val in = StringInputSource("OK...")
  //   val cmd = new ShellCommand(Vector("grep", "OK"), Map.empty, None, Some(in), None)
  //   val r = cmd.run
  //   val stdout = r.stdout
  //   println(stdout.toText)
  // }
}
