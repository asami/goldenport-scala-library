package org.goldenport.cli

import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * @since   Oct.  4, 2018
 *  version Oct. 21, 2018
 *  version Feb. 24, 2019
 * @version Mar.  4, 2019
 * @author  ASAMI, Tomoharu
 */
case class Request(
  service: Option[String],
  operation: String,
  arguments: List[Argument],
  switches: List[Switch],
  properties: List[Property]
) {
  def name = service.fold(operation)(x => s"$x:$operation")

  def withService(p: String) = copy(service = Some(p))
  def add(p: Argument) = copy(arguments = arguments :+ p)
  def add(p: Switch) = copy(switches = switches :+ p)
  def add(p: Property) = copy(properties = properties :+ p)

  def isVerbose: Boolean = switches.exists(_.name == "v")
  def isInteractive: Boolean = switches.exists(_.name == "i")

}

object Request {
  def apply(op: String): Request = Strings.totokens(op, ":") match {
    case Nil => Request(None, op, Nil, Nil, Nil)
    case x :: Nil => Request(None, x, Nil, Nil, Nil)
    case x :: xs => Request(Some(x), xs.mkString(":"), Nil, Nil, Nil)
  }

  def create(op: spec.Operation, args: Array[String]): Request = {
    @annotation.tailrec
    def go(req: Request, p: List[String]): Request = p match {
      case Nil => req
      case xs =>
        val (a, b) = op.parse(req, xs)
        go(a, b)
    }
    go(Request(op.name), args.toList)
  }
}
