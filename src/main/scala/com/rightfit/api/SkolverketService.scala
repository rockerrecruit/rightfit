package com.rightfit.api

import java.time.Instant

import com.rightfit.api.SkolverketService.Api.{SchoolSummary, SchoolUnitSerializeClass}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client._
import org.http4s.{EntityDecoder, EntityEncoder, Uri}
import zio.Task
import zio.interop.catz._

trait SkolverketService {
  def service: SkolverketService.Service[Any]
}

object SkolverketService {

  trait Service[R] {

    def getSchools(blazeClient: Client[Task[SchoolSummary]],
                   averageGrade: Int,
                   schoolName: String): Task[SchoolSummary] = {
      val EndPoint =
        "https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet"
      val baseUri = Uri.uri(EndPoint)
      blazeClient.expect[SchoolUnitSerializeClass](baseUri).map(_.toSchoolSummary)
    }
  }

  object Api {

    case class SchoolSummary(withdrawalDate: Instant,
                             footNote: SchoolSummary.FootNote,
                             schoolUnits: List[SchoolSummary.SchoolUnit])

    object SchoolSummary {
      import SchoolUnit._

      case class FootNote(value: String) extends AnyVal
      case class SchoolUnit(code: Code, name: Name, municipality: Municipality, orgNo: OrgNo)

      object SchoolUnit {
        case class Code(value: String)         extends AnyVal
        case class Name(value: String)         extends AnyVal
        case class Municipality(value: String) extends AnyVal
        case class OrgNo(value: String)        extends AnyVal
      }
    }

    case class SchoolUnitSerializeClass(Uttagsdatum: Instant,
                                        Fotnot: String,
                                        Skolenheter: List[SchoolUnitSerializeClass.Skolenhet]) {

      def toSchoolSummary: SchoolSummary = {
        import SchoolSummary.SchoolUnit._
        SchoolSummary(
          Uttagsdatum,
          SchoolSummary.FootNote(Fotnot),
          Skolenheter.map { enhet =>
            SchoolSummary.SchoolUnit(
              Code(enhet.Kommunkod),
              Name(enhet.Skolenhetsnamn),
              Municipality(enhet.Kommunkod),
              OrgNo(enhet.PeOrgNr)
            )
          }
        )
      }
    }

    object SchoolUnitSerializeClass {

      implicit def circeJsonDecoder[A](
        implicit decoder: Decoder[A]
      ): EntityDecoder[Task, A] = jsonOf[Task, A]

      implicit def circeJsonEncoder[A](
        implicit decoder: Encoder[A]
      ): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]

      implicit val e: Encoder[SchoolUnitSerializeClass] = semiauto.deriveEncoder
      implicit val d: Decoder[SchoolUnitSerializeClass] = semiauto.deriveDecoder

      case class Skolenhet(Skolenhetskod: String, Skolenhetsnamn: String, Kommunkod: String, PeOrgNr: String)

      object Skolenhet {
        implicit val e: Encoder[Skolenhet] = semiauto.deriveEncoder
        implicit val d: Decoder[Skolenhet] = semiauto.deriveDecoder
      }

    }

  }
}
