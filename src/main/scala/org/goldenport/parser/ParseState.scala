package org.goldenport.parser

import scalaz._, Scalaz._  
  
/*
 * @since   Aug. 20, 2018
 * @version Aug. 25, 2018
 * @author  ASAMI, Tomoharu
 */
trait ParseState[AST] {
  def apply(event: ParseEvent): (ParseState[AST], ParseResult[AST])
}

case class ParseStateClass[AST](
  init: ParseState[AST]
) {
  type B = ParseResult[AST]

  def action(event: ParseEvent) = State((s: ParseState[AST]) => s.apply(event))

  def parse(events: Seq[Char]): B = {
    val xs = CharEvent.make(events) :+ EndEvent
    val t = xs.traverseS(action)
    val r = t.eval(init)
    r.last
  }
}
