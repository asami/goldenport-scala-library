package org.goldenport.xml

import scala.xml._
import java.net.URI
import com.asamioffice.goldenport.text.UString
import dom.DomUtils

/*
 * @since   Aug. 13, 2017
 *  version Aug. 30, 2017
 * @version Oct. 25, 2017
 * @author  ASAMI, Tomoharu
 */
object XhtmlUtils {
  def parseNode(p: String): Node = DomUtils.toXml(DomUtils.parseHtml(p))

  def parseFragmentNode(p: String): Node = DomUtils.toXml(DomUtils.parseHtmlFragment(p))

  def anchorOrImgOrTextOrNodeOrEmpty(node: Option[NodeSeq], image: Option[URI], url: Option[URI]): NodeSeq =
    node.map(anchorOrImgOrTextOrNode(_, image, url)).getOrElse(Group(anchorOrImgOrEmpty(image, url)))

  def anchorOrImgOrTextOrNode(nodeseq: NodeSeq, image: Option[URI], url: Option[URI]): NodeSeq =
    nodeseq match {
      case Group(ms) => ms.toList match {
        case Nil => nodeseq
        case x :: Nil => anchorOrImgOrTextOrNode(x, image, url)
        case x :: xs => anchorOrImgOrNode(Group(nodeseq), image, url)
      }
      case m: Node => anchorOrImgOrTextOrNode(m, image, url)
      case _ => anchorOrImgOrTextOrNode(Group(nodeseq), image, url)
    }

  def anchorOrImgOrTextOrNode(node: Node, image: Option[URI], url: Option[URI]): Node =
    node match {
      case elem: Elem => elem.label.toLowerCase match {
        case "a" => makeImageInElement(updateHref(elem, url), image, elem.text)
        case "img" => updateSrc(updateHref(elem, url), image)
        case _ => anchorOrImgOrElement(elem, image, url)
      }
      case m: Text => anchorOrImgOrText(m, image, url)
      case _ => anchorOrImgOrNode(node, image, url)
    }

  def anchorOrImgOrEmpty(image: Option[URI], url: Option[URI]): Node =
    (image, url) match {
      case (Some(i), Some(u)) => anchor(i, u)
      case (Some(i), None) => img(i)
      case (None, Some(u)) => anchor(u)
      case (None, None) => Group(Nil)
    }

  def anchorOrImgOrElement(elem: Elem, image: Option[URI], url: Option[URI]): Elem =
    anchorOrImgOrNode(elem, image, url).asInstanceOf[Elem]

  def anchorOrImgOrText(t: Text, image: Option[URI], url: Option[URI]): Node =
    anchorOrImgOrNode(t, image, url)

  def anchorOrImgOrNode(node: Node, image: Option[URI], url: Option[URI]): Node =
    (image, url) match {
      case (Some(i), Some(u)) => anchor(node, u, i)
      case (Some(i), None) => img(i)
      case (None, Some(u)) => anchor(u)
      case (None, None) => node
    }

  // def imgOrTextOrEmpty(image: Option[URI], url: Option[URI]): Elem =
  //   <img url={url.toString}></img>

  def imgOrAnchor(image: URI, url: Option[URI]): Elem =
    url.fold(img(image))(anchor(image, _))

  def img(image: URI): Elem = <img src={image.toString}></img>

  // def anchor(node: Node, url: Option[URI], image: Option[URI]): Elem =
  //   anchor(node, image, url, elem.text)

  // // def anchor(node: Node, image: Option[URI], url: Option[URI], alt: String): Elem =
  // //   makeImageInElement(<a href={url.mkString}>{elem}</a>, image, alt)

  // def anchor(node: Node, url: Option[URI], image: Option[UR

  def anchor(node: Node, url: URI, image: URI): Elem =
    anchor(node, url, image, node.text)

  def anchor(node: Node, url: URI, image: URI, alt: String): Elem =
    <a href={url.toString}><img src={image.toString} alt={alt}></img></a>

  def anchor(url: URI, image: URI): Elem =
    <a href={url.toString}><img src={image.toString}></img></a>

  def anchor(url: URI): Elem =
    <a href={url.toString}>url.toString</a>

  def makeImageInElement(elem: Elem, image: Option[URI], alt: String): Elem = {
    val i = image.fold(<img alt={alt}></img>)(x => <img src={x.toString} alt={alt}></img>)
    elem.copy(child = elem.child :+ i)
  }

  def updateHref(elem: Elem, url: Option[URI]): Elem =
    XmlUtils.updateAttribute(elem, "href", url.map(_.toString))

  def updateSrc(elem: Elem, url: Option[URI]): Elem =
    XmlUtils.updateAttribute(elem, "src", url.map(_.toString))

  def title(s: String): Text = new Text(UString.capitalize(s))
}
