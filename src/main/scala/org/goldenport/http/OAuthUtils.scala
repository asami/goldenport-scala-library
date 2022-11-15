package org.goldenport.http

import org.goldenport.extension.IRecord

/*
 * @since   Nov. 15, 2022
 * @version Nov. 15, 2022
 * @author  ASAMI, Tomoharu
 */
object OAuthUtils {
  val PROP_AUTHORIZATION = "Authorization"
  val KIND_BEARER = "Bearer"

  def authorizationBearerTuple(accesstoken: String): (String, String) =
    PROP_AUTHORIZATION -> s"${KIND_BEARER} ${accesstoken}"

  def authorizationBearerRecord(accesstoken: String): IRecord =
    IRecord.data(authorizationBearerTuple(accesstoken))
}
