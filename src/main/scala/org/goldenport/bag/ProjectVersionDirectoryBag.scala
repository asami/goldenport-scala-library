package org.goldenport.bag

import java.io.File
import java.net.URL
import org.goldenport.values.Version
import org.goldenport.util.{StringUtils, ZipUtils}

/*
 * @since   Jul. 29, 2017
 * @version Aug. 29, 2017
 * @author  ASAMI, Tomoharu
 */
class ProjectVersionDirectoryBag(
  baseDirectory: File,
  homeDirectory: File,
  name: String,
  version: Option[Version]
) extends DirectoryBag(homeDirectory) {
}

object ProjectVersionDirectoryBag {
  def createFromZip(baseDirectory: File, url: URL): ProjectVersionDirectoryBag = {
    val (appname, version) = takeApplicationVersion(url)
    val isvolatile = version.fold(false)(_.isVolatile)
    val dirname = if (isvolatile)
      s"$appname-$version-${System.currentTimeMillis}"
    else
      s"$appname-$version"
    val home = new File(baseDirectory, dirname)
    if (!home.exists) {
      home.mkdirs
      ZipUtils.extract(home, url)
    }
    new ProjectVersionDirectoryBag(baseDirectory, home, appname, version)
  }

  def takeApplicationVersion(url: URL): (String, Option[Version]) = {
    val componentname = StringUtils.pathLastComponentBody(url.toString)
    componentname.indexOf('-') match {
      case -1 => (componentname, None)
      case i => (componentname.substring(0, i), Some(Version(componentname.substring(i + 1))))
    }
  }
}
