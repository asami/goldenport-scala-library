package org.goldenport.cli

import scalaz._, Scalaz._
import java.net.URL
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.Consequence
import org.goldenport.extension.IRecord

/*
 * @since   Oct.  4, 2018
 *  version Oct. 21, 2018
 *  version Feb. 24, 2019
 *  version Mar.  4, 2019
 *  version Feb. 16, 2020
 *  version May. 19, 2020
 *  version Apr. 25, 2021
 *  version Jan. 30, 2022
 *  version Feb.  1, 2022
 *  version Jan. 30, 2023
 * @version Jul. 23, 2023
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
  def addArguments(ps: Seq[Any]) = copy(arguments = arguments ++ ps.map(Argument.apply))

  def isVerbose: Boolean = switches.exists(_.name == "v")
  def isInteractive: Boolean = switches.exists(_.name == "i")

  def argumentsAsString: List[String] = arguments.map(_.asString)
  def argumentsAsUrl: List[URL] = arguments.map(_.asUrl)
  def arg1Url: URL = arguments.headOption.map(_.asUrl).getOrElse(RAISE.invalidArgumentFault("Missing argument"))

  def getArg1AsString: Option[String] = arguments.headOption.map(_.asString)
  def getArg1AsIntOrString: Option[Either[Int, String]] =
    arguments.headOption.map(_.asIntOrString)

  def getProperty(name: String): Option[Property] = properties.find(_.name == name)
  def getProperty(name: Symbol): Option[Property] = getProperty(name.name)
  def getPropertyString(name: String): Option[String] = properties.find(_.name == name).map(_.value.asString)

  def getUrlList(name: Symbol): Option[List[URL]] = getProperty(name).map(_.asUrlList)
  def asUrlList(name: Symbol): List[URL] = getUrlList(name) getOrElse Nil

  def consequenceArg1Url: Consequence[URL] =
    Consequence.executeOrMissingPropertyFault("argument1")(
      arguments.headOption.map(_.asUrl)
    )

  def consequenceArg1UrlOption: Consequence[Option[URL]] =
    Consequence(arguments.headOption.map(_.asUrl))

  def consequenceArg1ListingDirective: Consequence[ListingDirective] =
    Consequence.runOrMissingPropertyFault("argument1")(
      consequenceArg1ListingDirectiveOption
    )

  def consequenceArg1ListingDirectiveOption: Consequence[Option[ListingDirective]] =
    arguments.headOption.traverse(ListingDirective.parse)
  def consequenceArg1ListingDirectiveBaseOneOption: Consequence[Option[ListingDirective]] =
    arguments.headOption.traverse(ListingDirective.parseOne)
  def consequenceArg1ListingDirectiveBaseOneTailOption: Consequence[Option[ListingDirective]] =
    arguments.headOption.traverse(ListingDirective.parseOneTail)

  def toPropertyMap: Map[String, Any] =
    properties.foldLeft(Map.empty[String, Any])((z, x) => z + (x.name -> x.value.value))

  def toPropertyRecord: IRecord = IRecord.create(toPropertyMap)
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
