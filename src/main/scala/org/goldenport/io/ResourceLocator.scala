package org.goldenport.io

import java.io.File
import java.net.{URL, URI}

/*
 * @since   Jun. 24, 2019
 * @version Jun. 24, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait ResourceLocator {
}

case class FileResourceLocator(file: File) extends ResourceLocator {
}

case class UrlResourceLocator(url: URL) extends ResourceLocator {
}

case class UriResourceLocator(uri: URI) extends ResourceLocator {
}
