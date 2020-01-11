package com.rightfit.api

import cats.Show
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import zio._
import zio.interop.catz._

case class ScoreData(score: Int, interest: Interest)

object ScoreData {
  implicit val e: Encoder[ScoreData] = deriveEncoder
  implicit val d: Decoder[ScoreData] = deriveDecoder

}

case class Interest(value: String) extends AnyVal

object Interest {
  implicit val s: Show[Interest] = _.value
  implicit val e: Encoder[Interest] = deriveUnwrappedEncoder
  implicit val d: Decoder[Interest] = deriveUnwrappedDecoder
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

    HttpRoutes.of[ScoreTask] {
      case GET -> Root / IntVar(id) => Ok()
      case request @ POST -> Root =>
        request.decode[ScoreData] { json =>
          println(s"Read score data: [$json]")
          Ok()
        }
    }
  }
}
