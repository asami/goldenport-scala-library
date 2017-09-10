package org.goldenport.bag

/*
 * @since   Oct. 23, 2015
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
trait ReadOnlyFeature { self: ChunkBag =>
  def openOutputStream() = throw new UnsupportedOperationException("Read only")
}

