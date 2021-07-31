package org.goldenport.graphviz

import scala.collection.mutable.ArrayBuffer
import com.asamioffice.goldenport.text.TextBuilder

/*
 * @since   Jan. 14, 2009
 *  version Mar. 26, 2011
 *  version May.  4, 2020
 * @version Jul. 12, 2021
 * @author  ASAMI, Tomoharu
 */
class GVSubgraph(aId: String) extends GVElement(aId) {
  require (aId.startsWith("cluster"), aId + " does not start 'cluster' which is Graphviz convension") // Graphviz convension
  rankdir = "LR"
  val elements = new ArrayBuffer[GVElement]
  val edges = new ArrayBuffer[GVEdge]

  override def write(out: TextBuilder) {
    out.print("subgraph ")
    out.print(id)
    out.println(" {")
    out.indentUp
    print_attribute_lines(out)
    elements.foreach(_.write(out))
    edges.foreach(_.write(out))
    out.indentDown
    out.println("}")
  }
}
