package org.goldenport.values

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.Strings

/*
 * @since   Oct. 29, 2017
 *  version Jan.  3, 2019
 *  version Oct. 28, 2019
 * @version Feb. 29, 2020
 * @author  ASAMI, Tomoharu
 */
case class Urn(
  nid: String,
  module: String,
  components: List[String],
  rq_components: Option[String],
  fragment: Option[String]
) {
  lazy val nss = (module +: components).mkString(":")
  private def _rqcomponents = rq_components.getOrElse("")
  private def _fragment = fragment.fold("")(x => "#" + x)
  lazy val text = s"urn:$nid:${nss}${_rqcomponents}${_fragment}"
  lazy val submodule = components.headOption.getOrElse("")
  lazy val toURI = new URI(text)
}

object Urn {
  def apply(nid: String, module: String, components: String): Urn =
    apply(nid, module, components, "")
  def apply(nid: String, module: String, components: String, rqf: String): Urn = {
    val (_, rqcomponents, fragment) = _body_rqcomponents_fragment(rqf)
    Urn(nid, module, Strings.totokens(components, ":"), rqcomponents, fragment)
  }
  def apply(p: String): Urn = {
    val (body, rqcomponents, fragment) = _body_rqcomponents_fragment(p)
    val (nid, module, components) = {
      val a = Strings.totokens(body, ":")
      if (a.length <= 2)
        RAISE.invalidArgumentFault(s"Urn too short: $p")
      (a(1), a(2), a.drop(3))
    }
    Urn(nid, module, components, rqcomponents, fragment)
  }

  private def _body_rqcomponents_fragment(p: String): (String, Option[String], Option[String]) = {
    val rqindex = p.indexOf('?')
    val findex = p.indexOf('#')
    if (rqindex == -1) {
      if (findex == -1)
        (p, None, None)
      else
        (p.substring(0, findex), None, Some(p.substring(findex + 1)))
    } else {
      if (findex == -1)
        (p.substring(0, rqindex), Some(p.substring(rqindex)), None)
      else
        (p.substring(0, rqindex), Some(p.substring(rqindex, findex)), Some(p.substring(findex + 1)))
    }
  }
}
