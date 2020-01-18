package com.rightfit.api

import cats.Show
import cats.implicits.toShow
import com.rightfit.api.SkolverketService.{BlazeHttpClient, Live}
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import org.slf4j.LoggerFactory
import zio._
import zio.interop.catz._

case class ScoreData(score: Points, choice: PreferenceChoice)

object ScoreData {
  implicit val e: Encoder[ScoreData] = deriveEncoder
  implicit val d: Decoder[ScoreData] = deriveDecoder
}

case class Points(value: String) extends AnyVal

object Points {
  implicit val s: Show[Points]    = _.value
  implicit val e: Encoder[Points] = deriveUnwrappedEncoder
  implicit val d: Decoder[Points] = deriveUnwrappedDecoder
}

case class PreferenceChoice(value: String) extends AnyVal

object PreferenceChoice {
  implicit val s: Show[PreferenceChoice]    = _.value
  implicit val e: Encoder[PreferenceChoice] = deriveUnwrappedEncoder
  implicit val d: Decoder[PreferenceChoice] = deriveUnwrappedDecoder
}

final case class Api[R](rootUri: String) {

  type ScoreTask[A] = RIO[R, A]

  implicit def circeJsonDecoder[A](
    implicit decoder: Decoder[A]
  ): EntityDecoder[ScoreTask, A] = jsonOf[ScoreTask, A]

  implicit def circeJsonEncoder[A](
    implicit decoder: Encoder[A]
  ): EntityEncoder[ScoreTask, A] = jsonEncoderOf[ScoreTask, A]

  val dsl: Http4sDsl[ScoreTask] = Http4sDsl[ScoreTask]

  import dsl._

  def route: HttpRoutes[ScoreTask] = {
    val log = LoggerFactory.getLogger(getClass)

    HttpRoutes.of[ScoreTask] {
      case GET -> Root / "health"  => Ok()
      case GET -> Root / IntVar(_) => Ok()
      case request @ POST -> Root =>
        request.decode[ScoreData] { json =>
          for {
            c        <- BlazeHttpClient.client
            _        <- ZIO.effect(log.debug(s"Got request [$json]"))
            result   <- c.use(v => new Live[Any].service.getSchools(v, averageGrade = json.score.show.toDouble))
            response <- Ok(result)
          } yield response
        }
    }
  }
}
