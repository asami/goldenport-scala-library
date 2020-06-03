package org.goldenport.graphviz

import java.io._
// import org.goldenport.entity._
// import org.goldenport.entity.datasource.GDataSource
// import org.goldenport.entity.datasource.GContentDataSource
import com.asamioffice.goldenport.text.{AppendableTextBuilder, StringTextBuilder}

/*
 * Derived from GraphvizEntity.
 *
 * @since   Jan. 14, 2009
 *          Jan. 27, 2009
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
class Graphviz() {
  val graph: GVDigraph = new GVDigraph()

  // override protected def write_Content(out: BufferedWriter) {
  //   val builder = new AppendableTextBuilder(out)
  //   graph.write(builder)
  //   builder.flush()
  // }

  final def toDotText: String = {
    val builder = new StringTextBuilder
    graph.write(builder)
    builder.toString
  }
}

// class GraphvizEntityClass extends GEntityClass {
//   type Instance_TYPE = GraphvizEntity

//   override def accept_Suffix(suffix: String): Boolean = suffix == "dot"

//   override def reconstitute_DataSource(aDataSource: GDataSource, aContext: GEntityContext): Option[Instance_TYPE] = Some(new GraphvizEntity(aDataSource, aContext))
// }
