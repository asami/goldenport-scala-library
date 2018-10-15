package org.goldenport.xml.dom

import org.w3c.dom._

/*
 * @since   Apr. 11, 2016
 * @version Oct. 12, 2017
 * @author  ASAMI, Tomoharu
 */
trait DomTransformer extends DomTransformerHelper {
  def apply(node: Node): Node = transform_node(node)
}
