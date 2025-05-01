package org.goldenport.cli

import org.goldenport.cli.spec.{Parameter => SpecParameter}
import org.goldenport.context.ConsequenceValueHelper

/*
 * @since   Oct.  5, 2018
 *  version May. 19, 2019
 *  version Feb. 16, 2020
 *  version Apr. 25, 2021
 *  version Jan. 30, 2022
 *  version Jul. 22, 2023
 * @version Mar. 16, 2025
 * @author  ASAMI, Tomoharu
 */
case class Argument(
  name: String,
  value: Any,
  spec: Option[SpecParameter]
) extends Parameter with ConsequenceValueHelper {
  def isMatch(p: SpecParameter): Boolean = p.kind match {
    case SpecParameter.ArgumentKind => p.name == name
    case SpecParameter.SwitchKind => false
    case SpecParameter.PropertyKind => false
  }
}

object Argument {
  def apply(p: Any): Argument = Argument("", p, None)

  def apply(name: String, p: Any, spec: SpecParameter): Argument =
    new Argument(name, p, Some(spec))
}
