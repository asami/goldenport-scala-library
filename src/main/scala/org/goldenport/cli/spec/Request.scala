package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest, Switch, Argument}

/*
 * @since   Oct.  6, 2018
 * @version Oct. 10, 2018
 * @author  ASAMI, Tomoharu
 */
case class Request(
  parameters: List[Parameter]
) {
  def parse(req: CliRequest, args: List[String]): (CliRequest, List[String]) =
    args match {
      case Nil => (req, Nil)
      case _ => parameters.toStream.flatMap(_.parse(req, args)).headOption.getOrElse {
        case class Z(cr: CliRequest, xs: List[String]) {
          def r = (cr, xs)
          def isDone = xs.isEmpty
          def next = xs match {
            case Nil => this
            case x :: xs =>
              Z(Switch.get(x).map(cr.add).getOrElse(cr.add(Argument(x))), xs)
          }
        }
        @annotation.tailrec
        def go(z: Z): (CliRequest, List[String]) =
          if (z.isDone)
            z.r
          else
            go(z.next)
        go(Z(req, args))
      }
    }
}

object Request {
  val empty = Request(Nil)
}
