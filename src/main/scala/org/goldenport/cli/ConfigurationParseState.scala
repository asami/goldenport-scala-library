package org.goldenport.cli

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon}
import org.goldenport.context.Consequence
import org.goldenport.hocon.HoconUtils
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.util.NumberUtils

/*
 * @since   May. 10, 2025
 * @version May. 11, 2025
 * @author  ASAMI, Tomoharu
 */
case class ConfigurationParseState(
  args: Vector[String],
  hocon: Hocon
) {
  def complement(p: Hocon) = copy(hocon = hocon.withFallback(p))

  def toArgs: Array[String] = args.toArray

  object state {
    def getInt(key: String): (ConfigurationParseState, Option[Int]) =
      cIntOption(key).take

    def cStringOption(key: String): Consequence[(ConfigurationParseState, Option[String])] = {
      case class Z(
        args: Vector[String] = Vector.empty,
        property: Option[String] = None,
        value: Option[String] = None
      ) {
        def r = Consequence {
          val x = ConfigurationParseState.this.copy(args = args)
          val v = value orElse hocon.getStringOption(key)
          (x, v)
        }

        def +(rhs: String) = value match {
          case Some(s) => copy(args = args :+ rhs)
          case None => property match {
            case Some(ss) => copy(value = Some(rhs))
            case None => 
              if (rhs.startsWith("-")) {
                val name = rhs.dropWhile(_ == '-')
                if (name == key)
                  copy(property = Some(name))
                else
                  copy(args = args :+ rhs)
              } else {
                copy(args = args :+ rhs)
              }
            }
        }
      }
      args.foldLeft(Z())(_+_).r
    }

    def cIntOption(key: String): Consequence[(ConfigurationParseState, Option[Int])] =
      for {
        sv <- cStringOption(key)
        r <- {
          val (s, v) = sv
          v match {
            case Some(x) => NumberUtils.cIntProperty(key, v).map(x => (s, Some(x)))
            case None => Consequence.success(s, None)
          }
        }
      } yield r
  }
}

object ConfigurationParseState {
  case class Result[T](
    state: ConfigurationParseState,
    result: T
  ) {
    def toArgs = state.toArgs
  }

  val empty = ConfigurationParseState(Vector.empty, HoconUtils.empty)

  def apply(args: Array[String], hocon: Hocon): ConfigurationParseState =
    ConfigurationParseState(args.toVector, hocon)

  def apply(args: Array[String]): ConfigurationParseState =
    ConfigurationParseState(args.toVector, HoconUtils.empty)

  def apply(hocon: Hocon): ConfigurationParseState =
    ConfigurationParseState(Vector.empty, hocon)

  object StateFunctions {
    def getInt(key: String): State[ConfigurationParseState, Option[Int]] =
      State(_.state.getInt(key))

    def cStringOption(key: String): StateT[Consequence, ConfigurationParseState, Option[String]] =
      StateT(_.state.cStringOption(key))

    def cIntOption(key: String): StateT[Consequence, ConfigurationParseState, Option[Int]] =
      StateT(_.state.cIntOption(key))
  }
}
