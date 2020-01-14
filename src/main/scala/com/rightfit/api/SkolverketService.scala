package com.rightfit.api

import java.time.Instant

import cats.effect.Resource
import com.rightfit.api.SkolverketService.Api.{SchoolSummary, SchoolUnitJsonRep}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{EntityDecoder, EntityEncoder, Uri}
import zio.{Task, ZIO}
import zio.interop.catz._

trait SkolverketService {
  def service: SkolverketService.Service[Any]
}

object SkolverketService {

  trait Service[R] {
    def getSchools(blazeClient: Client[Task[SchoolSummary]], averageGrade: Int, schoolName: String): Task[SchoolSummary]
  }

//  final class Live[R] extends SkolverketService {
//
////    def getSchools(blazeClient: Client[Task[SchoolSummary]],
////                   averageGrade: Int,
////                   schoolName: String): Task[SchoolSummary] = {
////      val EndPoint =
////        "https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet"
////      val baseUri = Uri.uri(EndPoint)
////      blazeClient.expect[SchoolUnitJsonRep](baseUri).map(_.toSchoolSummary)
////
////    }
//
//    override def service: Service[Any] = {
//      override def getSchools(blazeClient: Client[Task[SchoolSummary]], averageGrade: Int, schoolName: String): Task[SchoolSummary] = ???
//
//      ???
//    }
//  }

  trait Live[R] extends SkolverketService {
    override def service: Service[Any] = new Service[R] {
      override def getSchools(blazeClient: Client[Task[SchoolSummary]],
                     averageGrade: Int,
                     schoolName: String): Task[SchoolSummary] = {
        val EndPoint =
          "https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet"
        val baseUri = Uri.uri(EndPoint)
        blazeClient.expect[SchoolUnitJsonRep](baseUri).map(_.toSchoolSummary)
      }
    }
  }

  object BlazeHttpClient {

    import zio._
    import zio.interop.catz._
    import org.http4s.client.blaze.BlazeClientBuilder

    def client: ZIO[Any, Nothing, Resource[Task, Client[Task]]] =
      ZIO.runtime.map { implicit r: Runtime[Any] =>
        BlazeClientBuilder[Task](r.platform.executor.asEC).resource
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

    case class SchoolUnitJsonRep(Uttagsdatum: Instant, Fotnot: String, Skolenheter: List[Skolenhet]) {

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

    object SchoolUnitJsonRep {
      implicit def circeJsonDecoder[A](
        implicit decoder: Decoder[A]
      ): EntityDecoder[Task, A] = jsonOf[Task, A]

      implicit def circeJsonEncoder[A](
        implicit decoder: Encoder[A]
      ): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]

      implicit val e: Encoder[SchoolUnitJsonRep] = semiauto.deriveEncoder
      implicit val d: Decoder[SchoolUnitJsonRep] = semiauto.deriveDecoder

    }

    case class Skolenhet(Skolenhetskod: String, Skolenhetsnamn: String, Kommunkod: String, PeOrgNr: String)

    object Skolenhet {
      implicit val e: Encoder[Skolenhet] = semiauto.deriveEncoder
      implicit val d: Decoder[Skolenhet] = semiauto.deriveDecoder
    }

  }
}
