package org.goldenport.xsv

import scalaz._, Scalaz._
import org.goldenport.Strings
import org.goldenport.context.Consequence
import org.goldenport.matrix.{IMatrix, Matrix}


/*
 * @since   Oct. 21, 2024
 *  version Oct. 22, 2024
 * @version Nov. 13, 2024
 * @author  ASAMI, Tomoharu
 */
case class LxsvSequence(
  vector: Vector[Lxsv]
) {
}

object LxsvSequence {
  def make(p: String): LxsvSequence = parse(p).take

  def parse(p: String): Consequence[LxsvSequence] = {
    val a = Strings.tolines(p)
    a.traverse(x => Consequence.from(Lxsv.parse(x))).map(LxsvSequence.apply)
  }
}

