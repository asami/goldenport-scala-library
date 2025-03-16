package org.goldenport.cli.spec

/*
 * @since   Oct.  6, 2018
 *  version Oct.  8, 2018
 *  version Feb. 24, 2019
 * @version Mar.  2, 2025
 * @author  ASAMI, Tomoharu
 */
case class Response(
  result: List[DataType] = Nil
) {
}

object Response {
  val empty = Response()
  val string = Response(XString)

  def apply(p: DataType, ps: DataType*): Response = Response(p :: ps.toList)
}
