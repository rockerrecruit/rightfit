package com.rightfit.model

import cats.Show
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}

case class County(value: String) extends AnyVal

object County {
  implicit val s: Show[County]    = _.value
  implicit val e: Encoder[County] = deriveUnwrappedEncoder
  implicit val d: Decoder[County] = deriveUnwrappedDecoder
}
