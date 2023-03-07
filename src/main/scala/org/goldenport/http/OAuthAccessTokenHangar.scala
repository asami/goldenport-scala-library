package org.goldenport.http

import org.goldenport.context.Consequence
import org.goldenport.extension.IRecord
import org.goldenport.util.TimedHangar

/*
 * @since   Nov. 12, 2022
 *  version Nov. 15, 2022
 *  version Dec. 27, 2022
 *  version Jan. 12, 2023
 * @version Mar.  7, 2023
 * @author  ASAMI, Tomoharu
 */
class OAuthAccessTokenHangar(
  initalstrategy: OAuthAccessTokenHangar.Strategy
) {
  import OAuthAccessTokenHangar.Strategy._

  private var _strategy: OAuthAccessTokenHangar.Strategy = initalstrategy

  def takeAccessToken(): Consequence[String] = _strategy.takeAccessToken()
  def takeNewAccessToken(): Consequence[String] = _strategy.takeNewAccessToken()

  def resetAuthCode(
    code: String,
    booter: AuthorizationCodeGrant.BootParameters => Consequence[AuthorizationCodeGrant.Info],
    refresher: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
  ): Consequence[String] = Consequence.run {
    _strategy = new AuthorizationCodeGrant(code, booter, refresher)
    takeNewAccessToken()
  }

  def resetRefreshToken(
    token: String,
    refresher: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
  ): Consequence[String] = {
    _strategy = new RefreshTokenGrant(token, refresher)
    refreshRefreshToken()
  }

  def refreshRefreshToken(): Consequence[String] = _strategy.refreshRefreshToken()

  def diagnostic(): Consequence[IRecord] = _strategy.diagnostic()
}

object OAuthAccessTokenHangar {
  sealed trait Strategy {
    import Strategy.AuthorizationCodeGrant._

    def name: String

    val cache = TimedHangar.create[Info]()

    def refreshDriver: RefreshParameters => Consequence[Info]

    protected def execute_Boot(): Consequence[String]

    def takeAccessToken(): Consequence[String] =
      Consequence.getOrRun(cache.get.map(_.access_token), takeNewAccessToken)

    def takeNewAccessToken(): Consequence[String] = _refresh(cache.getDeprecated)

    protected final def apply_info(info: Info): Consequence[String] = {
      cache.setSeconds(info, info.expires_in)
      Consequence.success(info.access_token)
    }

    def refreshRefreshToken(): Consequence[String] = {
      val info = cache.get orElse cache.getDeprecated
      _refresh(info)
    }

    private def _refresh(info: Option[Info]) =
      info match {
        case Some(s) => s.refresh_token match {
          case Some(token) => refreshDriver(RefreshParameters(token)) match {
            case Consequence.Success(info, _) => apply_info(info)
            case Consequence.Error(c) => Consequence.Error(c)
          }
          case None => execute_Boot()
        }
        case None => execute_Boot()
      }

    def diagnostic(): Consequence[IRecord] = Consequence(
      IRecord.data(
        "name" -> name,
        "token" -> _token
      )
    )

    private def _token = cache.get.map(_.diagnostic).getOrElse("empty")
  }
  object Strategy {
    class AuthorizationCodeGrant(
      val authorizationCode: String,
      val bootDriver: AuthorizationCodeGrant.BootParameters => Consequence[AuthorizationCodeGrant.Info],
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

      val name = "authorization-code-grant"

      protected def execute_Boot(): Consequence[String] =
        bootDriver(BootParameters(authorizationCode)) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.error(c)
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
        refresh_token: Option[String],
        token_type: String
      ) {
        def diagnostic = IRecord.data(
          "access_token" -> access_token,
          "expires_in" -> expires_in,
          "token_type" -> token_type
        ) + IRecord.dataOption(
          "refresh_token" -> refresh_token
        )
      }
    }

    class ClientCredentialsGrant(
      val parameters: ClientCredentialsGrant.Parameters,
      val bootDriver: ClientCredentialsGrant.Parameters => Consequence[AuthorizationCodeGrant.Info],
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]) extends Strategy {
      import ClientCredentialsGrant._

      val name = "client-credentials-grant"

      protected def execute_Boot(): Consequence[String] =
        bootDriver(parameters) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.error(c)
        }
    }
    object ClientCredentialsGrant {
      case class Parameters(
        clientId: String,
        clientSecrete: String
      )

      // case class Info(
      //   access_token: String,
      //   expires_in: Int,
      //   token_type: String
      // )
    }


    class RefreshTokenGrant(
      val refreshToken: String,
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

      val name = "refresh-token-grant"

      protected def execute_Boot(): Consequence[String] =
        refreshDriver(RefreshParameters(refreshToken)) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.Error(c)
        }
    }
    object RefreshTokenGrant {
    }

    class AccessTokenGrant(
      val accessToken: String
    ) extends Strategy {
      val name = "access-token-token-grant"
      val refreshDriver = _none_refresh_driver _

      protected def execute_Boot(): Consequence[String] = Consequence.success(accessToken)
    }
    object AccessTokenGrant {
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

  def clientCredentialsGrant(
    clientid: String,
    clientsecret: String,
    driver: Strategy.ClientCredentialsGrant.Parameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = clientCredentialsGrant(clientid, clientsecret, driver, _none_refresh_driver)

  def clientCredentialsGrant(
    clientid: String,
    clientsecret: String,
    driver: Strategy.ClientCredentialsGrant.Parameters => Consequence[Strategy.AuthorizationCodeGrant.Info],
    refreshDriver: Strategy.AuthorizationCodeGrant.RefreshParameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.ClientCredentialsGrant(Strategy.ClientCredentialsGrant.Parameters(clientid, clientsecret), driver, refreshDriver)
    new OAuthAccessTokenHangar(s)
  }

  def refreshTokenGrant(
    refreshToken: String,
    refreshDriver: Strategy.AuthorizationCodeGrant.RefreshParameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.RefreshTokenGrant(refreshToken, refreshDriver)
    new OAuthAccessTokenHangar(s)
  }

  def accessTokenGrant(
    accessToken: String
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.AccessTokenGrant(accessToken)
    new OAuthAccessTokenHangar(s)
  }

  private def _none_refresh_driver(p: Strategy.AuthorizationCodeGrant.RefreshParameters): Consequence[Strategy.AuthorizationCodeGrant.Info] =
    Consequence.unsupportedOperation("No refresh token handling")
}
