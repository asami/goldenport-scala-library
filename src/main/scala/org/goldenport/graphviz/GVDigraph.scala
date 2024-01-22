package org.goldenport.graphviz

import scala.collection.mutable.ArrayBuffer
import com.asamioffice.goldenport.text.TextBuilder
import org.goldenport.util.AnyUtils

/*
 * @since   Jan. 14, 2009
 *  version Mar. 18, 2009
 *  version May.  4, 2020
 * @version Jul. 12, 2021
 * @author  ASAMI, Tomoharu
 */
class GVDigraph {
  var compound: Option[Boolean] = Some(true)
  var rankdir: Option[String] = None // Some("LR")
  val defaultGraphAttributes = new GVGraphAttributes
  val defaultNodeAttributes = new GVNodeAttributes
  val defaultEdgeAttributes = new GVEdgeAttributes
  val elements = new ArrayBuffer[GVElement]
  val edges = new ArrayBuffer[GVEdge]

  final def write(out: TextBuilder) {
    out.println("digraph {")
    out.indentUp()
    out.println("newrank=true;")
    compound.foreach { x =>
      out.print("compond=")
      out.print(AnyUtils.toString(x))
      out.println(";")
    }
    rankdir.foreach { x =>
      out.print("rankdir=")
      out.print(x)
      out.println(";")
    }
    defaultGraphAttributes.write(out)
    defaultNodeAttributes.write(out)
    defaultEdgeAttributes.write(out)
    elements.foreach(_.write(out))
    edges.foreach(_.write(out))
    out.indentDown()
    out.println("}")
    out.flush()
  }
}

class GVGraphAttributes extends GVAttributeHolder {
  final def write(out: TextBuilder) {
    if (!is_attributes_avaliable) return
    out.print("graph")
    print_attributes(out)
    out.println(";")
  }
}

class GVNodeAttributes extends GVAttributeHolder {
  final def write(out: TextBuilder) {
    if (!is_attributes_avaliable) return
    out.print("node")
    print_attributes(out)
    out.println(";")
  }
}

class GVEdgeAttributes extends GVAttributeHolder {
  final def write(out: TextBuilder) {
    if (!is_attributes_avaliable) return
    out.print("edge")
    print_attributes(out)
    out.println(";")
  }
}
