package org.goldenport.values

import java.net.URI
import org.goldenport.Strings

/*
 * https://tools.ietf.org/html/rfc6350
 * https://en.wikipedia.org/wiki/VCard
 *
 * Curently, subset of vCard.
 * 
 * @since   Nov. 18, 2017
 *  version Nov. 24, 2017
 * @version Dec. 13, 2017
 * @author  ASAMI, Tomoharu
 */
case class VCard(
  n: Option[VName],
  fn: Option[VFullName],
  org: Option[VOrganization],
  title: Option[VTitle],
  photo: Option[VPhoto],
  adr: List[VAddress],
  tel: List[VTel],
  email: List[VEmail],
  rev: Option[VRevision]
)

// N:Forrest;Gump;;Mr.;
case class VName(
  surname: String,
  given: String,
  additional: Option[String],
  prefix: Option[String],
  suffix: Option[String]
)

// FN:Forrest Gump
case class VFullName(v: String)

// X.520
// ORG:Bubba Gump Shrimp Co.
case class VOrganization(
  title: List[String],
  role: List[String],
  logo: List[URI],
  org: List[String],
  member: List[String]
)

case class VTitle(v: String)

case class VPhoto(
  mediatype: String,
  uri: URI
)

// ADR;GEO="geo:12.3457,78.910";LABEL="Mr. John Q. Public, Esq.\n
//      Mail Drop: TNE QB\n123 Main Street\nAny Town, CA  91921-1234\n
//      U.S.A.":;;123 Main Street;Any Town;CA;91921-1234;U.S.A.
case class VAddress(
  param: Option[VParameters],
  pobox: Option[String],
  ext: Option[String],
  street: Option[String],
  locality: Option[String], // city
  region: Option[String], // state or province
  code: Option[String],
  country: Option[String]
)

case class VTel(value: String)

case class VEmail(value: String)

case class VRevision(value: String)

case class VParameters(
  label: Option[VLabel],
//  geo: Option[VGeo],
//  language: Option[VLanguage],
//  tz: Option[VTz],
  pref: Option[Int] // 0...100
)

case class VLabel(value: String)
