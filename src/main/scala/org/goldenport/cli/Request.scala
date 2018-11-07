package org.goldenport.cli

import org.goldenport.cli.spec.Operation

/*
 * @since   Oct.  4, 2018
 * @version Oct. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  arguments: List[Argument],
  switches: List[Switch],
  properties: List[Property]
) {
  def add(p: Argument) = copy(arguments = arguments :+ p)
  def add(p: Switch) = copy(switches = switches :+ p)
  def add(p: Property) = copy(properties = properties :+ p)

  def isVerbose: Boolean = ???
}

object Request {
  val empty = Request(Nil, Nil, Nil)

  def create(op: Operation, args: Array[String]): Request = {
    @annotation.tailrec
    def go(req: Request, p: List[String]): Request = p match {
      case Nil => req
      case xs =>
        val (a, b) = op.parse(req, xs)
        go(a, b)
    }
    go(Request.empty, args.toList)
  }
}
