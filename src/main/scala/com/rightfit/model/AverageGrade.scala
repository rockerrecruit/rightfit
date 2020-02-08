package com.rightfit.model

import cats.Show
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}

case class AverageGrade(value: Double) extends AnyVal

object AverageGrade {
  implicit val s: Show[AverageGrade]    = _.value.toString
  implicit val e: Encoder[AverageGrade] = deriveUnwrappedEncoder
  implicit val d: Decoder[AverageGrade] = deriveUnwrappedDecoder
}
