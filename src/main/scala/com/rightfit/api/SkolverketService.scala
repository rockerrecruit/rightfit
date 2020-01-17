package com.rightfit.api

import cats.Show
import cats.effect.Resource
import cats.implicits._
import com.rightfit.api.SkolverketService.Api.GymnasiumDetailedUnit.ProgramMetrics
import com.rightfit.api.SkolverketService.Api.GymnasiumDetailedUnit.ProgramMetrics.GeneralDataType
import com.rightfit.api.SkolverketService.Api.SchoolSummary.SchoolUnit
import com.rightfit.api.SkolverketService.Api.{GymnasiumDetailedUnit, SchoolUnitJsonRep}
import com.rightfit.api.SkolverketService.{BlazeHttpClient, Live}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{EntityDecoder, EntityEncoder, Uri}
import zio._
import zio.interop.catz._

import scala.util.Try

trait SkolverketService[R] {
  def service: SkolverketService.Service[R]
}

object SkolverketService {

  sealed trait AppError
  case class General(message: String) extends AppError

  trait Service[R] {
    def getSchools(blazeClient: Client[Task], averageGrade: Int, schoolName: String): Task[List[Api.GymnasiumUnit]]
  }

  final class Live[R] extends SkolverketService[R] {
    override def service: Service[R] =
      (blazeClient: Client[Task], avg: Int, schoolName: String) => {
        val e1                               = Uri.unsafeFromString("https://api.scb.se/UF0109/v2/skolenhetsregister/sv/skolenhet?gy")
        val e2                               = (unit: String) => Uri.unsafeFromString(s"https://api.scb.se/school-units/$unit/statistics/gy")
        val schoolEndPoint: String => String = (unit: String) => s"/school-units/{schoolUnitCode}/statistics/gy/$unit"
        for {
          schoolSummary <- blazeClient.expect[SchoolUnitJsonRep](e1).map(_.toSchoolSummary)
          schoolsWithAvg <- ZIO.foreach(schoolSummary.schoolUnits) { unit =>
                             blazeClient
                               .expect[GymnasiumDetailedUnit](e2(unit.code.value))
                               .map(_.toGymnasiumUnit(unit))
                           //.mapError(e => General(e.getMessage))
                           }
          relevantSchools = schoolsWithAvg.collect {
            case gymnasiumUnit if gymnasiumUnit.admissionAvg + 10 >= avg && gymnasiumUnit.admissionAvg - 10 <= avg =>
              gymnasiumUnit
          }
        } yield relevantSchools
      }
  }

  object BlazeHttpClient {

    def client: ZIO[Any, Nothing, Resource[Task, Client[Task]]] =
      ZIO.runtime.map { implicit runtime: Runtime[Any] =>
        BlazeClientBuilder[Task](runtime.platform.executor.asEC).resource
      }
  }

  object Api {

    case class PotentialSchools(schools: List[SchoolSummary.SchoolUnit])

    case class SchoolSummary(withdrawalDate: String,
                             footNote: SchoolSummary.FootNote,
                             schoolUnits: List[SchoolSummary.SchoolUnit])

    object SchoolSummary {

      implicit val s: Show[SchoolSummary] = summary => {
        val units = summary.schoolUnits.map(unit => s"$unit\n").combineAll
        s"Withdrawal date: ${summary.withdrawalDate}\nFootnote: ${summary.footNote.value}\nSchoolUnits: \n$units"
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

    case class GymnasiumUnit(schoolUnit: SchoolUnit, admissionAvg: Int)

    case class GymnasiumDetailedUnit(
      certifiedTeachersQuota: List[GeneralDataType],
      programMetrics: List[ProgramMetrics],
      specialEducatorsQuota: List[GeneralDataType],
      specialTeachersPositions: List[GeneralDataType],
      studentsPerTeacherQuota: List[GeneralDataType],
      totalNumberOfPupils: List[GeneralDataType],
    ) {

      def toGymnasiumUnit(schoolUnit: SchoolUnit): GymnasiumUnit = {
        val avgGrade = (for {
          metric   <- programMetrics
          avg      <- metric.admissionPointAverage
          intValue = Try(avg.value.toInt).toOption
        } yield intValue).flatten.sum

        GymnasiumUnit(schoolUnit, avgGrade)
      }
    }

    object GymnasiumDetailedUnit {

      implicit val e: Encoder[GymnasiumDetailedUnit]                                         = semiauto.deriveEncoder
      implicit val d: Decoder[GymnasiumDetailedUnit]                                         = semiauto.deriveDecoder
      implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]
      implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]

      import ProgramMetrics._
      case class ProgramMetrics(
        admissionPointAverage: List[GeneralDataType],
        admissionPointMin: List[GeneralDataType],
        averageResultNationalTestsSubjectENG: List[GeneralDataType],
        averageResultNationalTestsSubjectMA1: List[GeneralDataType],
        averageResultNationalTestsSubjectMA2: List[GeneralDataType],
        averageResultNationalTestsSubjectSVA: List[GeneralDataType],
        averageResultNationalTestsSubjectSVE: List[GeneralDataType],
        certifiedTeachersQuota: List[GeneralDataType],
        docLinks: List[DocLinks],
        engSubjectTest: String,
        gradePointsForStudents: List[GeneralDataType],
        gradesPointsForStudentsWithExam: List[GeneralDataType],
        hasLibrary: Boolean,
        links: List[Link],
        ma1SubjectTest: String,
        ma2SubjectTest: String,
        programCode: String,
        ratioOfPupilsWithExamWithin3Years: List[GeneralDataType],
        ratioOfStudentsEligibleForUndergraduateEducation: List[GeneralDataType],
        schoolUnit: String,
        specialEducatorsQuota: List[GeneralDataType],
        specialTeachersPositions: List[GeneralDataType],
        studentsPerTeacherQuota: List[GeneralDataType],
        svaSubjectTest: String,
        sveSubjectTest: String,
        totalNumberOfPupils: List[GeneralDataType],
      )

      object ProgramMetrics {
        case class GeneralDataType(timePeriod: String, value: String, valueType: String)
        case class DocLinks(description: String, title: String, uri: String)
        case class Link(deprecation: String,
                        href: String,
                        hreflang: String,
                        media: String,
                        rel: String,
                        templated: Boolean,
                        title: String,
                        `type`: String)

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
            .flatMap(gymnasiumUnits => zio.console.putStrLn(gymnasiumUnits.toString))
    } yield 0).catchAllCause(cause => zio.console.putStrLn(s"${cause.prettyPrint}").as(1))
  }

}
