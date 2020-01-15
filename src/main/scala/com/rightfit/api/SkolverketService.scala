package com.rightfit.api

import cats.Show
import cats.effect.Resource
import cats.implicits._
import com.rightfit.api.SkolverketService.Api.{SchoolSummary, SchoolUnitJsonRep}
import com.rightfit.api.SkolverketService.{BlazeHttpClient, Live}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{EntityDecoder, EntityEncoder, Uri}
import zio._
import zio.interop.catz._

trait SkolverketService[R] {
  def service: SkolverketService.Service[R]
}

object SkolverketService {

  trait Service[R] {
    def getSchools(blazeClient: Client[Task], averageGrade: Int, schoolName: String): Task[SchoolSummary]
  }

  final class Live[R] extends SkolverketService[R] {
    override def service: Service[R] =
      (blazeClient: Client[Task], averageGrade: Int, schoolName: String) => {
        val EndPoint = "https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet"
        val baseUri  = Uri.unsafeFromString(EndPoint)
        blazeClient.expect[SchoolUnitJsonRep](baseUri).map(_.toSchoolSummary)
      }
  }

  object BlazeHttpClient {

    def client: ZIO[Any, Nothing, Resource[Task, Client[Task]]] =
      ZIO.runtime.map { implicit runtime: Runtime[Any] =>
        BlazeClientBuilder[Task](runtime.platform.executor.asEC).resource
      }
  }

  object Api {

    case class SchoolSummary(withdrawalDate: String,
                             footNote: SchoolSummary.FootNote,
                             schoolUnits: List[SchoolSummary.SchoolUnit])

    object SchoolSummary {

      implicit val s: Show[SchoolSummary] = summary => {
        val units = summary.schoolUnits.map(unit => s"$unit\n").combineAll
        s"Withdrawal date: ${summary.withdrawalDate.value}\nFootnote: ${summary.footNote.value}\nSchoolUnits: \n$units"
      }
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

    case class SchoolUnitJsonRep(Uttagsdatum: String, Fotnot: String, Skolenheter: List[Skolenhet]) {

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
      implicit val e: Encoder[SchoolUnitJsonRep]                                             = semiauto.deriveEncoder
      implicit val d: Decoder[SchoolUnitJsonRep]                                             = semiauto.deriveDecoder
      implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]
      implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]
    }

    case class Skolenhet(Skolenhetskod: String, Skolenhetsnamn: String, Kommunkod: String, PeOrgNr: String)

    object Skolenhet {
      implicit val e: Encoder[Skolenhet] = semiauto.deriveEncoder
      implicit val d: Decoder[Skolenhet] = semiauto.deriveDecoder
    }

  }

}

object TestBlazeHttpClient extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    (for {
      c <- BlazeHttpClient.client
      _ <- c.use(v => new Live[Any].service.getSchools(v, averageGrade = 5, schoolName = "FakeSchool"))
            .flatMap(summary => zio.console.putStrLn(summary.show))
    } yield 0).catchAllCause(cause => zio.console.putStrLn(s"${cause.prettyPrint}").as(1))
  }

}
