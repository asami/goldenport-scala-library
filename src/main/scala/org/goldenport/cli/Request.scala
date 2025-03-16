package org.goldenport.cli

import scalaz._, Scalaz._
import java.io.File
import java.net.URL
import com.typesafe.config.{Config => Hocon}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.util.StringUtils
import org.goldenport.context.Consequence
import org.goldenport.context.Faults
import org.goldenport.io.InputSource
import org.goldenport.hocon.HoconUtils
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
 *  version Jul. 23, 2023
 * @version Mar. 16, 2025
 * @author  ASAMI, Tomoharu
 */
case class Request(
  service: Option[String],
  operation: String,
  arguments: List[Argument],
  switches: List[Switch],
  properties: List[Property]
) {
  import Request._

  def name = service.fold(operation)(x => s"$x:$operation")

  def withService(p: String) = copy(service = Some(p))
  def add(p: Argument) = copy(arguments = arguments :+ p)
  def add(p: Switch) = copy(switches = switches :+ p)
  def add(p: Property) = copy(properties = properties :+ p)
  def addArguments(ps: Seq[Any]) = copy(arguments = arguments ++ ps.map(Argument.apply))

  def isVerbose: Boolean = switches.exists(_.name == "v")
  def isInteractive: Boolean = switches.exists(_.name == "i")

  def get(p: spec.Parameter): Option[Any] = 
    p.kind match {
      case spec.Parameter.ArgumentKind => properties.find(_.isMatch(p)).map(_.value) orElse arguments.headOption.map(_.value)
      case spec.Parameter.SwitchKind => switches.find(_.isMatch(p)).map(_.value)
      case spec.Parameter.PropertyKind => properties.find(_.isMatch(p)).map(_.value)
    }

  def list(p: spec.Parameter): List[Any] = listOption(p) getOrElse Nil

  def listOption(p: spec.Parameter): Option[List[Any]] = get(p) map {
    case m: Array[_] => m.toList
    case m: Seq[_] => m.toList
    case m: String => StringUtils.eagerCommaForm(m)
    case m => List(m)
  }

  // def sParameter(p: spec.Parameter): (Request, ParameterValue[Any]) = ???

  // def sFile(p: spec.Parameter): (Request, ParameterValue[File]) = ???

  // def sConfig(p: spec.Parameter): (Request, ParameterValue[Hocon]) = ???

  def cAny(p: spec.Parameter): Consequence[Any] =
    Consequence.successOrMissingArgumentFault(p.name, get(p))

  def cAnyOption(p: spec.Parameter): Consequence[Option[Any]] =
    Consequence(get(p))

  def cAnyList(p: spec.Parameter): Consequence[List[Any]] =
    Consequence(list(p))

  def cAnyListOption(p: spec.Parameter): Consequence[Option[List[Any]]] =
    Consequence(listOption(p))

  def cFile(p: spec.Parameter): Consequence[File] = for {
    x <- cAny(p)
    r <- p.cFile(x)
  } yield r

  def cConfig(p: spec.Parameter): Consequence[Hocon] = for {
    x <- cAny(p)
    r <- p.cConfig(x)
  } yield r

  def cConfigOrZero(p: spec.Parameter): Consequence[Hocon] = for {
    x <- cAnyOption(p)
    r <- x match {
      case Some(s) => p.cConfig(s)
      case None => Consequence.success(HoconUtils.empty)
    }
  } yield r

  def cInputSourceList(p: spec.Parameter): Consequence[List[InputSource]] = for {
    xs <- cAnyList(p)
    r <- p.cInputSourceList(xs)
  } yield r

  def argumentsAsString: List[String] = arguments.map(_.asString)
  def argumentsAsUrl: List[URL] = arguments.map(_.asUrl)
  def arg1Url: URL = arguments.headOption.map(_.asUrl).getOrElse(RAISE.invalidArgumentFault("Missing argument"))

  def getArg1AsString: Option[String] = arguments.headOption.map(_.asString)
  def getArg1AsIntOrString: Option[Either[Int, String]] =
    arguments.headOption.map(_.asIntOrString)

  def getProperty(name: String): Option[Property] = properties.find(_.name == name)
  def getProperty(name: Symbol): Option[Property] = getProperty(name.name)
  def getPropertyString(name: String): Option[String] = properties.find(_.name == name).map(_.asString)

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
    properties.foldLeft(Map.empty[String, Any])((z, x) => z + (x.name -> x.value))

  def toPropertyRecord: IRecord = IRecord.create(toPropertyMap)
}

object Request {
  // case class ParameterValue[T](
  //   parameter: Option[spec.Parameter],
  //   value: Consequence[T]
  // )

  // type ParseState[T] = State[Request, T]

  // object Parser {
  //   def parameter(p: spec.Parameter): ParseState[Any] = State { req =>
  //     req.sParameter(p)
  //   }

  //   def file(p: spec.Parameter): State[Request, ParameterValue[File]] = State { req =>
  //     val value: (Request, ParameterValue[File]) = req.sFile(p)
  //     (value._1, value._2)
  //   }

  //   def config(p: spec.Parameter): State[Request, ParameterValue[Hocon]] = State { req =>
  //     val value: (Request, ParameterValue[Hocon]) = req.sConfig(p)
  //     (value._1, value._2)
  //   }
  // }

  // object Test {
  //   def x = {
  //     import Request.Parser._

  //     val a = for {
  //       in <- file(spec.Parameter.argumentFile("in")).map(identity)
  //       config <- config(spec.Parameter.propertyConfigFileOrEmpty()).map(identity)
  //     } yield {
  //       ???
  //     }
  //   }
  // }

  // sealed trait ParseState[T] {
  //     def next[U](p: ParseState[U]): ParseState[U]
  // }
  // object ParseState {
  //   case class Success[T](request: Request, value: T) extends ParseState[T] {
  //     def map[U](f: T => U): ParseState[U] = ???
  //     def flatMap[U](f: T => ParseState[U]): ParseState[U] = ???

  //     def next[U](p: ParseState[U]): ParseState[U] = {
  //       ???
  //     }
  //   }
  //   case class Failure[T](request: Request, faults: Faults) extends ParseState[T] {
  //     def map[U](f: T => U): ParseState[U] = ???
  //     def flatMap[U](f: T => ParseState[U]): ParseState[U] = ???

  //     def next[U](p: ParseState[U]): ParseState[U] = {
  //       ???
  //     }
  //   }
  // }
  // case class Parser[T](state: ParseState[T]) {
  //   def map[U](f: ParseState[T] => ParseState[U]): Parser[U] =
  //     Parser[U](state.next(f(state)))
  //   def flatMap[U](f: ParseState[T] => Parser[U]): Parser[U] = {
  //     val a: ParseState[U] = f(state).state
  //     Parser(state.next(f(state).state))
  //   }
  // }
  // object Parser {
  //   for {
  //     a <- Parser(ParseState.Success(Request.empty, 1))
  //   } yield a
  // }

  def apply(op: String): Request = Strings.totokens(op, ":") match {
    case Nil => Request(None, op, Nil, Nil, Nil)
    case x :: Nil => Request(None, x, Nil, Nil, Nil)
    case x :: xs => Request(Some(x), xs.mkString(":"), Nil, Nil, Nil)
  }

  def apply(op: String, arg: Argument, prop: Property): Request =
    Request(None, op, List(arg), Nil, List(prop))

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
