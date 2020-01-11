package com.rightfit.api

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import zio._
import zio.interop.catz._

final case class Api[R](rootUri: String) {

  type UserTask[A] = RIO[R, A]

  implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[UserTask, A] = jsonOf[UserTask, A]

  implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[UserTask, A] = jsonEncoderOf[UserTask, A]

  case class ScoreData(score: Int)

  object ScoreData {
    implicit val e: Encoder[ScoreData] = deriveEncoder
    implicit val d: Decoder[ScoreData] = deriveDecoder

}

  val dsl: Http4sDsl[UserTask] = Http4sDsl[UserTask]

  import dsl._

  def route: HttpRoutes[UserTask] = {

    HttpRoutes.of[UserTask] {
      case GET -> Root / IntVar(id) => Ok()
      case request @ POST -> Root =>
        request.decode[ScoreData] { json =>
          println(s"Read score data: [$json]")
          Ok()
        }
    }
  }
}

