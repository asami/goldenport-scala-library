package org.goldenport.cli.spec

import org.goldenport.cli.{Request => CliRequest, Switch, Argument}

/*
 * @since   Oct.  6, 2018
 *  version Oct. 10, 2018
 * @version Feb. 16, 2020
 * @author  ASAMI, Tomoharu
 */
case class Request(
  parameters: List[Parameter]
) {
  def parse(req: CliRequest, args: Seq[String]): (CliRequest, List[String]) =
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
        go(Z(req, args.toList))
      }
    }

  def build(req: CliRequest, args: Seq[Any]): CliRequest = {
    case class Z(rq: CliRequest = req, as: List[Any] = args.toList) {
      def r = rq.addArguments(as) // XXX

      def +(rhs: Parameter) = {
        val (x, y) = rhs.build(rq, as)
        Z(x, y)
      }
    }
    parameters./:(Z())(_+_).r
  }
    // args.toList match {
    //   case Nil => req
    //   case _ => parameters.toStream.flatMap(_.build(req, args)).headOption.getOrElse {
    //     case class Z(cr: CliRequest, xs: List[Any]) {
    //       def r = (cr, xs)
    //       def isDone = xs.isEmpty
    //       def next = xs match {
    //         case Nil => this
    //         case x :: xs => x match {
    //           case m: String => Z(Switch.get(m).map(cr.add).getOrElse(cr.add(Argument(x))), xs)
    //           case _ => Z(cr.add(Argument(x))
    //         }
    //       }
    //     }
    //     @annotation.tailrec
    //     def go(z: Z): (CliRequest, List[Any]) =
    //       if (z.isDone)
    //         z.r
    //       else
    //         go(z.next)
    //     go(Z(req, args.toList))
    //   }
    // }
}

object Request {
  val empty = Request(Nil)
}
