package org.goldenport.http

import org.goldenport.context.Consequence
import org.goldenport.util.TimedHangar

/*
 * @since   Nov. 12, 2022
 *  version Nov. 15, 2022
 *  version Dec. 27, 2022
 * @version Jan. 10, 2023
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
}

object OAuthAccessTokenHangar {
  sealed trait Strategy {
    import Strategy.AuthorizationCodeGrant._

    val cache = TimedHangar.create[Info]()

    def refreshDriver: RefreshParameters => Consequence[Info]

    protected def execute_Boot(): Consequence[String]

    def takeAccessToken(): Consequence[String] =
      Consequence.getOrRun(cache.get.map(_.access_token), takeNewAccessToken)

    def takeNewAccessToken(): Consequence[String] =
      cache.getDeprecated match {
        case Some(s) => refreshDriver(RefreshParameters(s.refresh_token)) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.Error(c)
        }
        case None => execute_Boot()
      }

    protected final def apply_info(info: Info): Consequence[String] = {
      cache.setSeconds(info, info.expires_in)
      Consequence.success(info.access_token)
    }

    def refreshRefreshToken(): Consequence[String] = {
      val a = cache.get orElse cache.getDeprecated
      a match {
        case Some(s) => refreshDriver(RefreshParameters(s.refresh_token)) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.Error(c)
        }
        case None => execute_Boot()
      }
    }
  }
  object Strategy {
    class AuthorizationCodeGrant(
      val authorizationCode: String,
      val bootDriver: AuthorizationCodeGrant.BootParameters => Consequence[AuthorizationCodeGrant.Info],
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

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
        refresh_token: String,
        token_type: String
      )
    }

    class RefreshTokenGrant(
      val refreshToken: String,
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]
    ) extends Strategy {
      import AuthorizationCodeGrant._

      protected def execute_Boot(): Consequence[String] =
        refreshDriver(RefreshParameters(refreshToken)) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.Error(c)
        }
    }
    object RefreshTokenGrant {
    }

    class ClientCredentialGrant(
      val parameters: ClientCredentialGrant.Parameters,
      val bootDriver: ClientCredentialGrant.Parameters => Consequence[AuthorizationCodeGrant.Info],
      val refreshDriver: AuthorizationCodeGrant.RefreshParameters => Consequence[AuthorizationCodeGrant.Info]) extends Strategy {
      import ClientCredentialGrant._

      protected def execute_Boot(): Consequence[String] =
        bootDriver(parameters) match {
          case Consequence.Success(info, _) => apply_info(info)
          case Consequence.Error(c) => Consequence.error(c)
        }
    }
    object ClientCredentialGrant {
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
    driver: Strategy.ClientCredentialGrant.Parameters => Consequence[Strategy.AuthorizationCodeGrant.Info],
    refreshDriver: Strategy.AuthorizationCodeGrant.RefreshParameters => Consequence[Strategy.AuthorizationCodeGrant.Info]
  ): OAuthAccessTokenHangar = {
    val s = new Strategy.ClientCredentialGrant(Strategy.ClientCredentialGrant.Parameters(clientid, clientsecret), driver, refreshDriver)
    new OAuthAccessTokenHangar(s)
  }
}
