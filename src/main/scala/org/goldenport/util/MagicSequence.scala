package org.goldenport.util

/*
 * @since   Mar. 17, 2025
 * @version Mar. 17, 2025
 * @author  ASAMI, Tomoharu
 */
case class MagicSequence(
  delimiter: Option[Char],
  data: Vector[String]
) {
  def toList: List[String] = data.toList
  def toVector: Vector[String] = data.toVector
}

object MagicSequence {
  val delimiters = Vector(',', ';', ' ', '\t')

  def create(p: String): MagicSequence = {
    val s = p.trim
    val a = delimiters.map(x => x -> s.indexOf(x)).sortBy(_._2)
    val b = a.last
    if (b._2 == -1) {
      MagicSequence(None, Vector(s))
    } else {
      val delimiter = b._1
      MagicSequence(Some(delimiter), StringUtils.splitNormalizedToVector(delimiter, p))
    }
  }

  def toList(p: String): List[String] = create(p).toList

  def toVector(p: String): Vector[String] = create(p).toVector
}
