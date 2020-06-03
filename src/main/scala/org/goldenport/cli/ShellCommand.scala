package org.goldenport.cli

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import java.io.{File, InputStream, IOException}
import org.goldenport.Strings
import org.goldenport.io.{InputSource, StringInputSource}
import org.goldenport.bag.{ChunkBag, BufferFileBag}

/*
 * @since   Oct.  4, 2018
 *  version Oct.  8, 2018
 * @version May. 21, 2020
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

  def run: Boolean = execute.waitFor == 0

  def execute: ShellCommand.Result = {
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

    def toText: String = toChunkBag.toText

    def toChunkBag: ChunkBag = {
      waitFor match {
        case 0 => stdout
        case n =>
          val msg = stderr.toText
          throw new IOException(msg)
      }
    }

    def toChunkBag(name: String): ChunkBag = toChunkBag.withFilename(name)

    def toChunkBag(name: Option[String]): ChunkBag = name.map(toChunkBag).getOrElse(toChunkBag)
  }

  def create(cmds: String): ShellCommand = new ShellCommand(
    Strings.totokens(cmds, " "),
    Map.empty,
    None,
    None,
    None
  )

  def create(cmds: String, in: InputSource): ShellCommand = new ShellCommand(
    Strings.totokens(cmds, " "),
    Map.empty,
    None,
    Some(in),
    None
  )

  def execute(cmds: String): Result = create(cmds).execute

  def execute(cmds: String, in: InputSource): Result = create(cmds, in).execute

  def execute(cmds: String, in: String): Result = create(cmds, StringInputSource(in)).execute

  def run(cmds: String): Boolean = create(cmds).run

  def runAsString(cmds: String): String = runAsChunkBag(cmds).toText

  def runAsString(cmds: String, in: InputSource): String = runAsChunkBag(cmds, in).toText

  def runAsString(cmds: String, in: String): String = runAsChunkBag(cmds, StringInputSource(in)).toText

  def runAsChunkBag(cmds: String): ChunkBag = execute(cmds).toChunkBag

  def runAsChunkBag(cmds: String, in: InputSource): ChunkBag = execute(cmds, in).toChunkBag

  def runAsChunkBag(cmds: String, in: String): ChunkBag = execute(cmds, StringInputSource(in)).toChunkBag

  // def main(args: Array[String]) {
  //   val in = StringInputSource("OK...")
  //   val cmd = new ShellCommand(Vector("grep", "OK"), Map.empty, None, Some(in), None)
  //   val r = cmd.run
  //   val stdout = r.stdout
  //   println(stdout.toText)
  // }
}
