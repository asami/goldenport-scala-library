package org.goldenport.graphviz

import com.asamioffice.goldenport.text.TextBuilder

/*
 * @since   Jan. 14, 2009
 *  version Jan. 29, 2009
 *  version May.  4, 2020
 * @version Jul. 11, 2021
 * @author  ASAMI, Tomoharu
 */
class GVEdge(
  val fromId: String,
  val fromPort: String,
  val toId: String,
  val toPort: String,
  val ankerId: Option[String] = None
) extends GVAttributeHolder {
  require (fromId != null && fromPort != null && toId != null && toPort != null)

  final def write(out: TextBuilder) {
    out.print(fromId)
    if (fromPort != "") {
      out.print(":")
      out.print(fromPort)
    }
    out.print(" -> ")
    ankerId match {
      case Some(aid) =>
        out.print(aid)
        if (toPort != "") {
          out.print(":")
          out.print(toPort) // XXX
        }
      case None => 
        out.print(toId)
        if (toPort != "") {
          out.print(":")
          out.print(toPort)
        }
    }
    print_attributes(out)
    out.println(";")
  }
}
