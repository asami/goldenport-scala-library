package org.goldenport.cli.spec

/*
 * @since   Oct.  6, 2018
 * @version Oct.  8, 2018
 * @author  ASAMI, Tomoharu
 */
case class Service(
  name: String,
  operations: Seq[Operation]
) {
}

object Service {
}
