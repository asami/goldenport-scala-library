package org.goldenport.http

import org.goldenport.context.Consequence
import org.goldenport.util.TimedHangar

/*
 * @since   Nov. 12, 2022
 * @version Nov. 15, 2022
 * @author  ASAMI, Tomoharu
 */
class OAuthAccessTokenHangar(
  val strategy: OAuthAccessTokenHangar.Strategy
) {
  def takeAccessToken(): Consequence[String] = strategy.takeAccessToken()
  def takeNewAccessToken(): Consequence[String] = strategy.takeNewAccessToken()
}

object OAuthAccessTokenHangar {
  sealed trait Strategy {
    def takeAccessToken(): Consequence[String]
    def takeNewAccessToken(): Consequence[String]
  }
  object Strategy {
    class AuthorizationCodeGrant(
      val authorizationCode: String,
      val bootDriver: AuthorizationCodeGrant.BootParameters => Consequence[AuthorizationCodeGrant.Info],
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

      val cache = TimedHangar.create[Info]()

      def takeAccessToken(): Consequence[String] =
        Consequence.getOrRun(cache.get.map(_.access_token), takeNewAccessToken)

      def takeNewAccessToken(): Consequence[String] =
        cache.getDeprecated match {
          case Some(s) => refreshDriver(RefreshParameters(s.refresh_token)) match {
            case Consequence.Success(info, _) => _apply_info(info)
            case Consequence.Error(c) => _execute_boot()
          }
          case None => _execute_boot()
        }

      private def _execute_boot(): Consequence[String] =
        bootDriver(BootParameters(authorizationCode)) match {
          case Consequence.Success(info, _) => _apply_info(info)
          case Consequence.Error(c) => Consequence.error(c)
        }

      private def _apply_info(info: Info): Consequence[String] = {
        cache.setSeconds(info, info.expires_in)
        Consequence.success(info.access_token)
      }
    }
    object AuthorizationCodeGrant {
      case class BootParameters(
        authorizationCode: String
      )
      case class RefreshParameters(
        refreshToken: String
      )

      case class Info(
        access_token: String,
        expires_in: Int,
        refresh_token: String,
        token_type: String
      )
    }

    class RefreshTokenGrant(
      val refreshToken: String,
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

      val cache = TimedHangar.create[Info]()

      def takeAccessToken(): Consequence[String] =
        Consequence.getOrRun(cache.get.map(_.access_token), takeNewAccessToken)

      def takeNewAccessToken(): Consequence[String] =
        cache.getDeprecated match {
          case Some(s) => refreshDriver(RefreshParameters(s.refresh_token)) match {
            case Consequence.Success(info, _) => _apply_info(info)
            case Consequence.Error(c) => Consequence.Error(c)
          }
          case None => _execute_boot()
        }

      private def _execute_boot(): Consequence[String] =
        refreshDriver(RefreshParameters(refreshToken)) match {
          case Consequence.Success(info, _) => _apply_info(info)
          case Consequence.Error(c) => Consequence.Error(c)
        }

      private def _apply_info(info: Info): Consequence[String] = {
        cache.setSeconds(info, info.expires_in)
        Consequence.success(info.access_token)
      }
    }
    object RefreshTokenGrant {
    }

    class ClientCredentialGrant(
      val parameters: ClientCredentialGrant.Parameters,
      val driver: ClientCredentialGrant.Parameters => Consequence[ClientCredentialGrant.Info]
    ) extends Strategy {
      import ClientCredentialGrant._

      val cache = TimedHangar.create[ClientCredentialGrant.Info]()

      def takeAccessToken(): Consequence[String] =
        Consequence.getOrRun(cache.get.map(_.access_token), takeNewAccessToken)

      def takeNewAccessToken(): Consequence[String] = 
        driver(parameters) match {
          case Consequence.Success(info, _) =>
            cache.setSeconds(info, info.expires_in)
            Consequence.success(info.access_token)

          case Consequence.Error(c) => Consequence.Error(c)
        }
    }
    object ClientCredentialGrant {
      case class Parameters(
        clientId: String,
        clientSecrete: String
      )

      case class Info(
        access_token: String,
        expires_in: Int,
        token_type: String
      )
    }
  }

  def authorizationCodeGrant(
    authorizationCode: String,
    bootDriver: Strategy.AuthorizationCodeGrant.BootParameters => Consequence[Strategy.AuthorizationCodeGrant.Info],
    refreshDriver: Strategy.AuthorizationCodeGrant.RefreshParameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.AuthorizationCodeGrant(authorizationCode, bootDriver, refreshDriver)
    new OAuthAccessTokenHangar(s)
  }

  def refreshTokenGrant(
    refreshToken: String,
    refreshDriver: Strategy.AuthorizationCodeGrant.RefreshParameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.RefreshTokenGrant(refreshToken, refreshDriver)
    new OAuthAccessTokenHangar(s)
  }

  def clientCredentialGrant(
    clientid: String,
    clientsecret: String,
    driver: Strategy.ClientCredentialGrant.Parameters => Consequence[Strategy.ClientCredentialGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.ClientCredentialGrant(Strategy.ClientCredentialGrant.Parameters(clientid, clientsecret), driver)
    new OAuthAccessTokenHangar(s)
  }
}
