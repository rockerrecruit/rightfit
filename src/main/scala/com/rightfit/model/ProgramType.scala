package com.rightfit.model

import cats.Show
import cats.syntax.show._
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.semiauto

sealed trait ProgramType

object ProgramType {

  case object NaturalScience              extends ProgramType
  case object Economics                   extends ProgramType
  case class Miscellaneous(value: String) extends ProgramType

  def programTypes: List[ProgramType] = List(NaturalScience, Economics)

  def apply(programType: String): ProgramType = {
    programTypes.filter(_.show == programType) match {
      case head :: _ => head
      case Nil       => Miscellaneous(programType)
    }
  }

  implicit val s: Show[ProgramType] = {
    case NaturalScience       => "NA"
    case Economics            => "EK"
    case Miscellaneous(value) => value
  }

  implicit val e: Encoder[ProgramType] = Encoder[String].contramap(_.show)
  implicit val d: Decoder[ProgramType] = Decoder.instance(_.as[String].map(ProgramType.apply))

}
