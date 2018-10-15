package org.goldenport.bag

import org.goldenport.io.RangeInputStream

/*
 * @since   Oct.  5, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
case class ChunkBagView(
  bag: ChunkBag,
  start: Long,
  end: Option[Long] // exclude
) {
  def copyTo[T <: ChunkBag](to: T): T = {
    resource.managed(bag.openInputStream()) acquireAndGet { in =>
      val rin = new RangeInputStream(in, start, end)
      to.write(rin)
    }
    to
  }
}
