package org.goldenport.io

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.util.StringUtils

/*
 * @since   Sep.  1, 2020
 * @version Sep.  8, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class MimeTypeSpec extends WordSpec with Matchers with GivenWhenThen {
  "getBySuffix" should {
    "pdf" in {
      MimeType.getBySuffix("pdf").map(_.name) should be(Some("application/pdf"))
    }
    "xls" in {
      MimeType.getBySuffix("xls").map(_.name) should be(Some("application/vnd.ms-excel"))
    }
    "xlsx" in {
      MimeType.getBySuffix("xlsx").map(_.name) should be(Some("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
    }
    "ppt" in {
      MimeType.getBySuffix("ppt").map(_.name) should be(Some("application/vnd.ms-powerpoint"))
    }
    "pptx" in {
      MimeType.getBySuffix("pptx").map(_.name) should be(Some("application/vnd.openxmlformats-officedocument.presentationml.presentation"))
    }
    "doc" in {
      MimeType.getBySuffix("doc").map(_.name) should be(Some("application/msword"))
    }
    "docx" in {
      MimeType.getBySuffix("docx").map(_.name) should be(Some("application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
    }
  }
}
