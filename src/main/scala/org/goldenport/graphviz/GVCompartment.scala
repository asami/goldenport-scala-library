package org.goldenport.graphviz

import scala.collection.mutable.ArrayBuffer

/*
 * Jan. 17, 2009
 * Jan. 18, 2009
 * @since   
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
class GVCompartment {
  val lines = new ArrayBuffer[GVCompartmentLine]

  final def addLine(aLabel: String): GVCompartmentLine = {
    val line = new GVCompartmentLine
    line.label = aLabel
    lines += line
    line
  }
}

class GVCompartmentLine {
  var label: String = ""
  var fontsize: String = ""
  var align: String = "center"

  final def align_is(anAlign: String): GVCompartmentLine = {
    align = anAlign
    this
  }
}
