package com.rightfit.api

import com.rightfit.api.SkolverketService.Api.SchoolSummary
import com.rightfit.api.SkolverketService.Api.SchoolSummary.School
import org.http4s.client.Client
import zio.{Task, ZIO}

trait SkolverketService {
  def service: SkolverketService.Service[Any]
}

object SkolverketService {
  trait Service[R] {

    def getSchools(blazeClient: Client[Task[SchoolSummary]], averageGrade: Int, schoolName: String): ZIO[R, String, SchoolSummary] = {
      import org.http4s._
      val EndPoint = "https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet"
      val baseUri = Uri.uri(EndPoint)

      blazeClient.expect[SchoolSummary](baseUri)

      ???
    }
  }

  object Api {
    case class SchoolSummary(schools: List[School])

    object SchoolSummary {
      case class School(id: String, name: String)
    }

  }
}


/*


package se.bynk.tink.api

import cats.effect.Effect
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.io._
import org.http4s.headers.{Accept, Authorization}
import se.bynk.tink.api.ApiConfig.Bootstrap
import se.bynk.tink.api.command._

trait ApiAlgebra[F[_]] {
  def getAccessToken(authorizationCode: String): F[AuthResponse]
  def getAccounts(auth: AuthResponse): F[TinkBankAccountSummary]
}

object ApiAlgebra {

  class ApiClient[F[_]: Effect](client: Client[F], bootstrap: Bootstrap) extends ApiAlgebra[F] with Http4sClientDsl[F] {

    import bootstrap._

    override def getAccessToken(authorizationCode: String): F[AuthResponse] = {
      val EndPoint = "oauth/token"
      val post = POST(
        UrlForm(
          "code"          -> authorizationCode,
          "client_id"     -> clientId.value,
          "client_secret" -> clientSecret.value,
          "grant_type"    -> "authorization_code"
        ),
        buildUri(EndPoint)
      )

      client.expect[AuthResponse](post)
    }

    override def getAccounts(auth: AuthResponse): F[TinkBankAccountSummary] = {
      val EndPoint = "accounts/list"
      val request = GET(
        buildUri(EndPoint),
        Authorization(Credentials.Token(AuthScheme.Bearer, auth.access_token)),
        Accept(MediaType.application.json)
      )
      client.expect[TinkBankAccountSummary](request)
    }

    private def buildUri(endpoint: String): Uri = {
      val baseUri = Uri.unsafeFromString(baseUrl.value)
      baseUri.withPath(s"${baseUri.path}/$endpoint")
    }

  }

  object ApiClient {
    def apply[F[_]: Effect](client: Client[F], bs: Bootstrap): ApiClient[F] = new ApiClient[F](client, bs)
  }

}
 */