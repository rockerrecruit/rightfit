package com.rightfit.api

import cats.Show
import cats.syntax.show._
import com.rightfit.api.skolverket.Api.GymnasiumDetailedUnit
import com.rightfit.api.skolverket.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import com.rightfit.api.skolverket.SkolverketClient
import com.rightfit.model.{AverageGrade, County, ProgramType}
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import org.slf4j.LoggerFactory
import zio._
import zio.interop.catz._

import scala.util.Try

object Server {
  case class ScoreData(score: Points, programType: ProgramType, county: County)

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

}

final case class Server[R](rootUri: String) {
  import Server._

  type ScoreTask[A] = RIO[R, A]

  implicit def circeJsonDecoder[A](implicit dec: Decoder[A]): EntityDecoder[ScoreTask, A] = jsonOf[ScoreTask, A]
  implicit def circeJsonEncoder[A](implicit dec: Encoder[A]): EntityEncoder[ScoreTask, A] = jsonEncoderOf[ScoreTask, A]

  val dsl: Http4sDsl[ScoreTask] = Http4sDsl[ScoreTask]
  import dsl._

  def route(schools: List[(SchoolUnitRep, GymnasiumDetailedUnit)]): HttpRoutes[ScoreTask] = {
    val log = LoggerFactory.getLogger(getClass)

    HttpRoutes.of[ScoreTask] {
      case GET -> Root / "health"  => Ok()
      case GET -> Root / IntVar(_) => Ok()
      case request @ POST -> Root =>
        request.decode[ScoreData] { scoreData =>
          for {
            _        <- ZIO.effect(log.debug(s"Got request [$scoreData]"))
            avgGrade <- ZIO.fromTry(Try(AverageGrade(scoreData.score.value.toDouble)))
            result    = SkolverketClient.getSchoolByGrade(schools, avgGrade, scoreData.county, scoreData.programType)
            _        <- ZIO.effect(log.debug(s"Responding client with: ${result.show}"))
            _        <- ZIO.effect(log.debug(s"${result.schools.take(10)}"))
            response <- Ok(result)
          } yield response
        }
    }
  }
}
