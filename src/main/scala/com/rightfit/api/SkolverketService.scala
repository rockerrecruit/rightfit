package com.rightfit.api

import cats.Show
import cats.implicits._
import cats.effect.Resource
import com.rightfit.api.SkolverketService.Api.{GymnasiumDetailedUnit, PotentialSchools}
import com.rightfit.api.SkolverketService.Api.GymnasiumUnit.SchoolUnit
import com.rightfit.api.SkolverketService.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import com.rightfit.api.SkolverketService.{BlazeHttpClient, Live}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import zio._
import zio.interop.catz._

import scala.util.Try

trait SkolverketService[R] {
  def service: SkolverketService.Service[R]
}

object SkolverketService {

  trait Service[R] {
    def getSchools(blazeClient: Client[Task], averageGrade: Double): Task[PotentialSchools]
  }

  final class Live[R] extends SkolverketService[R] {
    override def service: Service[R] =
      (blazeClient: Client[Task], avg: Double) => {

        val e1 =
          Uri.unsafeFromString("https://api.skolverket.se/planned-educations/school-units?size=3&typeOfSchooling=gy")

        val req = Request[Task](
          Method.GET,
          e1,
          headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        )

        val e2 = (unit: String) =>
          Uri.unsafeFromString(s"https://api.skolverket.se/planned-educations/school-units/$unit/statistics/gy")

        val req2 = (uri: Uri) =>
          Request[Task](
            Method.GET,
            uri,
            headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        )

        for {
          schoolSummary  <- blazeClient.expect[Api.SchoolUnitSummary](req)
          schoolsWithAvg <- ZIO.foreach(schoolSummary.body._embedded.listedSchoolUnits) { u =>
                             blazeClient.expect[GymnasiumDetailedUnit](req2(e2(u.code))).map(_.toGymnasiumUnit(u))
                            }
          relevantSchools = schoolsWithAvg.collect {
                              case gymnasiumUnit if gymnasiumUnit.isWithin10Avg(avg) => gymnasiumUnit
                            }
        } yield PotentialSchools(relevantSchools)
      }
  }

  object BlazeHttpClient {

    def client: ZIO[Any, Nothing, Resource[Task, Client[Task]]] =
      ZIO.runtime.map { implicit runtime: Runtime[Any] =>
        BlazeClientBuilder[Task](runtime.platform.executor.asEC).resource
      }
  }

  object Api {

    case class PotentialSchools(schools: List[GymnasiumUnit])

    object PotentialSchools {
      implicit val s: Show[PotentialSchools] = potentialSchools => {
        potentialSchools.schools.map { school =>
          s"School: ${school.schoolUnit.name} has average admission: ${school.admissionAvg}\n"
        }.combineAll
      }
    }

    case class GymnasiumUnit(schoolUnit: SchoolUnit, admissionAvg: Double) {
      def isWithin10Avg(averageTarget: Double): Boolean =
        admissionAvg + 150.0 >= averageTarget && admissionAvg - 150.0 <= averageTarget
    }

    object GymnasiumUnit {
      import SchoolUnit._
      case class SchoolUnit(code: Code, name: Name, municipality: Municipality, orgNo: OrgNo)

      object SchoolUnit {
        case class Code(value: String)         extends AnyVal
        case class Name(value: String)         extends AnyVal
        case class Municipality(value: String) extends AnyVal
        case class OrgNo(value: String)        extends AnyVal
      }
    }

    case class GymnasiumDetailedUnit(status: String, message: String, body: GymnasiumDetailedUnit.Body) {

      def toGymnasiumUnit(schoolUnit: SchoolUnitRep): GymnasiumUnit = {

        def parseCommaString(string: String): Option[Double] = {
          import java.text.NumberFormat
          import java.util.Locale
          Try {
            val format = NumberFormat.getInstance(Locale.FRANCE)
            val number = format.parse(string)
            number.doubleValue
          }.toOption
        }

        val avgGrade = (for {
          metric   <- body.programMetrics.toList
          avg      <- metric.admissionPointsAverage.toList
          value    = avg.value.flatMap(parseCommaString)
        } yield value).flatten.sum

        val unit = SchoolUnit(
          SchoolUnit.Code(schoolUnit.code),
          SchoolUnit.Name(schoolUnit.name),
          SchoolUnit.Municipality(schoolUnit.code),
          SchoolUnit.OrgNo(schoolUnit.code),
        )

        GymnasiumUnit(unit, avgGrade)
      }
    }

    object GymnasiumDetailedUnit {

      implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]
      implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]
      implicit val e: Encoder[GymnasiumDetailedUnit]                                         = semiauto.deriveEncoder
      implicit val d: Decoder[GymnasiumDetailedUnit]                                         = semiauto.deriveDecoder

      case class SchoolValues(value: Option[String], valueType: String, timePeriod: Option[String])

      object SchoolValues {
        implicit val e: Encoder[SchoolValues] = semiauto.deriveEncoder
        implicit val d: Decoder[SchoolValues] = semiauto.deriveDecoder
      }

      case class Body(
        programMetrics: Seq[ProgramMetrics],
        specialTeacherPositions: Seq[SchoolValues],
        studentsPerTeacherQuota: Seq[SchoolValues],
        certifiedTeachersQuota: Seq[SchoolValues],
        specialEducatorsQuota: Seq[SchoolValues],
        totalNumberOfPupils: Seq[SchoolValues]
      )

      object Body {
        implicit val e: Encoder[Body] = semiauto.deriveEncoder
        implicit val d: Decoder[Body] = semiauto.deriveDecoder
      }

      case class Links(self: Self)

      object Links {
        implicit val e: Encoder[Links] = semiauto.deriveEncoder
        implicit val d: Decoder[Links] = semiauto.deriveDecoder
      }

      case class ProgramMetrics(
        programCode: String,
        averageResultNationalTestsSubjectSVE: Seq[SchoolValues],
        sveSubjectTest: Option[String],
        averageResultNationalTestsSubjectSVA: Seq[SchoolValues],
        svaSubjectTest: Option[String],
        averageResultNationalTestsSubjectMA1: Seq[SchoolValues],
        ma1SubjectTest: Option[String],
        averageResultNationalTestsSubjectMA2: Seq[SchoolValues],
        ma2SubjectTest: Option[String],
        averageResultNationalTestsSubjectENG: Seq[SchoolValues],
        engSubjectTest: Option[String],
        schoolUnit: String,
        specialTeacherPositions: Seq[SchoolValues],
        studentsPerTeacherQuota: Seq[SchoolValues],
        certifiedTeachersQuota: Seq[SchoolValues],
        docLinks: Option[String],
        hasLibrary: Boolean,
        totalNumberOfPupils: Seq[SchoolValues],
        ratioOfStudentsEligibleForUndergraduateEducation: Seq[SchoolValues],
        gradesPointsForStudents: Seq[SchoolValues],
        gradesPointsForStudentsWithExam: Seq[SchoolValues],
        ratioOfPupilsWithExamWithin3Years: Seq[SchoolValues],
        admissionPointsMin: Seq[SchoolValues],
        admissionPointsAverage: Seq[SchoolValues],
        specialEducatorsQuota: Seq[SchoolValues],
        _links: Links
      )

      object ProgramMetrics {
        implicit val e: Encoder[ProgramMetrics] = semiauto.deriveEncoder
        implicit val d: Decoder[ProgramMetrics] = semiauto.deriveDecoder
      }

      case class Self(href: String)

      object Self {
        implicit val e: Encoder[Self] = semiauto.deriveEncoder
        implicit val d: Decoder[Self] = semiauto.deriveDecoder
      }

    }

    case class SchoolUnitSummary(status: String, message: String, body: SchoolUnitSummary.Body)

    object SchoolUnitSummary {

      implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]
      implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]
      implicit val e: Encoder[SchoolUnitSummary]                                             = semiauto.deriveEncoder
      implicit val d: Decoder[SchoolUnitSummary]                                             = semiauto.deriveDecoder

      case class Body(_embedded: Body.Embedded, _links: Body.Links, page: Body.Page)

      object Body {
        implicit val e: Encoder[Body] = semiauto.deriveEncoder
        implicit val d: Decoder[Body] = semiauto.deriveDecoder

        case class Page(size: Int, totalElements: Int, totalPages: Int, number: Int)

        object Page {
          implicit val e: Encoder[Page] = semiauto.deriveEncoder
          implicit val d: Decoder[Page] = semiauto.deriveDecoder
        }

        case class Links(first: Links.First, self: Links.Self, next: Links.Next, last: Links.Last)

        object Links {

          implicit val e: Encoder[Links] = semiauto.deriveEncoder
          implicit val d: Decoder[Links] = semiauto.deriveDecoder

          case class First(href: String)

          object First {
            implicit val e: Encoder[First] = semiauto.deriveEncoder
            implicit val d: Decoder[First] = semiauto.deriveDecoder
          }

          case class Self(href: String)

          object Self {
            implicit val e: Encoder[Self] = semiauto.deriveEncoder
            implicit val d: Decoder[Self] = semiauto.deriveDecoder
          }

          case class Next(href: String)

          object Next {
            implicit val e: Encoder[Next] = semiauto.deriveEncoder
            implicit val d: Decoder[Next] = semiauto.deriveDecoder
          }

          case class Last(href: String)

          object Last {
            implicit val e: Encoder[Last] = semiauto.deriveEncoder
            implicit val d: Decoder[Last] = semiauto.deriveDecoder
          }

        }

        case class Embedded(listedSchoolUnits: List[Embedded.SchoolUnitRep])

        object Embedded {
          implicit val e: Encoder[Embedded] = semiauto.deriveEncoder
          implicit val d: Decoder[Embedded] = semiauto.deriveDecoder

          case class SchoolUnitRep(
            code: String,
            geographicalAreaCode: String,
            _links: SchoolUnitRep.Link,
            name: String,
            //          postCodeDistrict: String,
            //          principalOrganizerType: String,
            //          studentsPerTeacherQuota: String,
            //          typeOfSchooling: List[SchoolUnitRep.TypeOfSchooling]
          )

          object SchoolUnitRep {

            implicit val e: Encoder[SchoolUnitRep] = semiauto.deriveEncoder
            implicit val d: Decoder[SchoolUnitRep] = semiauto.deriveDecoder

            case class Link(
              self: Link.Self,
              statistics: Link.Statistics
            )

            object Link {
              implicit val e: Encoder[Link] = semiauto.deriveEncoder
              implicit val d: Decoder[Link] = semiauto.deriveDecoder

              case class Self(href: String)

              object Self {
                implicit val e: Encoder[Self] = semiauto.deriveEncoder
                implicit val d: Decoder[Self] = semiauto.deriveDecoder
              }

              case class Statistics(href: String)

              object Statistics {
                implicit val e: Encoder[Statistics] = semiauto.deriveEncoder
                implicit val d: Decoder[Statistics] = semiauto.deriveDecoder
              }
            }

            case class TypeOfSchooling(
              code: String,
              displayName: String,
              schoolYears: List[String]
            )

            object TypeOfSchooling {
              implicit val e: Encoder[TypeOfSchooling] = semiauto.deriveEncoder
              implicit val d: Decoder[TypeOfSchooling] = semiauto.deriveDecoder
            }

          }

        }
      }

    }

  }

}

object TestBlazeHttpClient extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    (for {
      c <- BlazeHttpClient.client
      _ <- c.use(v => new Live[Any].service.getSchools(v, averageGrade = 240.0))
            .flatMap(potentialSchools => zio.console.putStrLn(potentialSchools.show))
    } yield 0).catchAllCause(cause => zio.console.putStrLn(s"${cause.prettyPrint}").as(1))
  }

}
