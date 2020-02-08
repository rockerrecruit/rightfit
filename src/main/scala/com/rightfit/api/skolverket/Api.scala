package com.rightfit.api.skolverket

import cats.Show
import io.circe.generic.extras.semiauto.{deriveUnwrappedDecoder, deriveUnwrappedEncoder}
import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.http4s.{EntityDecoder, EntityEncoder}
import zio.Task
import cats.implicits._
import com.rightfit.api.skolverket.Api.GymnasiumUnit.SchoolUnit
import com.rightfit.api.skolverket.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import com.rightfit.model.{AverageGrade, County}
import org.http4s.circe._
import zio.interop.catz._

import scala.util.Try

object Api {

  case class PotentialSchools(schools: List[GymnasiumUnit])

  object PotentialSchools {
    implicit val e: Encoder[PotentialSchools] = semiauto.deriveEncoder
    implicit val d: Decoder[PotentialSchools] = semiauto.deriveDecoder

    implicit val s: Show[PotentialSchools] = potentialSchools => {
      potentialSchools.schools match {
        case _ :: _ =>
          potentialSchools.schools
            .map(school => s"School: ${school.schoolUnit.name.value} has average admission: ${school.admissionAvg}\n")
            .prepended("\n")
            .combineAll
        case Nil =>
          s"No potential schools found."
      }
    }
  }

  case class GymnasiumUnit(schoolUnit: SchoolUnit, admissionAvg: Double) {

    def isWithin10Avg(averageTarget: AverageGrade): Boolean =
      admissionAvg + 10.0 >= averageTarget.value && admissionAvg - 10.0 <= averageTarget.value

    def isInCounty(county: County): Boolean = {
      schoolUnit.municipality.value.startsWith(county.value)
    }
  }

  object GymnasiumUnit {

    implicit val e: Encoder[GymnasiumUnit] = semiauto.deriveEncoder
    implicit val d: Decoder[GymnasiumUnit] = semiauto.deriveDecoder

    import SchoolUnit._

    case class SchoolUnit(code: Code, name: Name, municipality: Municipality, orgNo: OrgNo)

    object SchoolUnit {

      implicit val e: Encoder[SchoolUnit] = semiauto.deriveEncoder
      implicit val d: Decoder[SchoolUnit] = semiauto.deriveDecoder

      case class Code(value: String) extends AnyVal

      object Code {
        implicit val e: Encoder[Code] = deriveUnwrappedEncoder
        implicit val d: Decoder[Code] = deriveUnwrappedDecoder
      }

      case class Name(value: String) extends AnyVal

      object Name {
        implicit val e: Encoder[Name] = deriveUnwrappedEncoder
        implicit val d: Decoder[Name] = deriveUnwrappedDecoder
      }

      case class Municipality(value: String) extends AnyVal

      object Municipality {
        implicit val e: Encoder[Municipality] = deriveUnwrappedEncoder
        implicit val d: Decoder[Municipality] = deriveUnwrappedDecoder
      }

      case class OrgNo(value: String) extends AnyVal

      object OrgNo {
        implicit val e: Encoder[OrgNo] = deriveUnwrappedEncoder
        implicit val d: Decoder[OrgNo] = deriveUnwrappedDecoder
      }

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

      val avgList = (for {
        metric      <- body.programMetrics.toList
        avg         <- metric.admissionPointsAverage.toList
        maybeDouble = avg.value.flatMap(parseCommaString)
      } yield maybeDouble).flatten

      val avgGrade = avgList.sum / avgList.size

      val unit = SchoolUnit(
        SchoolUnit.Code(schoolUnit.code),
        SchoolUnit.Name(schoolUnit.name),
        SchoolUnit.Municipality(schoolUnit.geographicalAreaCode),
        SchoolUnit.OrgNo(schoolUnit.code),
      )

      GymnasiumUnit(unit, avgGrade)
    }
  }

  object GymnasiumDetailedUnit {

    implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[Task, A] = jsonOf[Task, A]
    implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[Task, A] = jsonEncoderOf[Task, A]

    implicit val e: Encoder[GymnasiumDetailedUnit] = semiauto.deriveEncoder
    implicit val d: Decoder[GymnasiumDetailedUnit] = semiauto.deriveDecoder

    case class Body(
      programMetrics: Seq[Body.ProgramMetrics],
      specialTeacherPositions: Seq[Body.SchoolValues],
      studentsPerTeacherQuota: Seq[Body.SchoolValues],
      certifiedTeachersQuota: Seq[Body.SchoolValues],
      specialEducatorsQuota: Seq[Body.SchoolValues],
      totalNumberOfPupils: Seq[Body.SchoolValues]
    )

    object Body {
      implicit val e: Encoder[Body] = semiauto.deriveEncoder
      implicit val d: Decoder[Body] = semiauto.deriveDecoder

      case class SchoolValues(value: Option[String], valueType: String, timePeriod: Option[String])

      object SchoolValues {
        implicit val e: Encoder[SchoolValues] = semiauto.deriveEncoder
        implicit val d: Decoder[SchoolValues] = semiauto.deriveDecoder
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
        _links: ProgramMetrics.Links
      )

      object ProgramMetrics {
        implicit val e: Encoder[ProgramMetrics] = semiauto.deriveEncoder
        implicit val d: Decoder[ProgramMetrics] = semiauto.deriveDecoder

        case class Links(self: Links.Self)

        object Links {
          implicit val e: Encoder[Links] = semiauto.deriveEncoder
          implicit val d: Decoder[Links] = semiauto.deriveDecoder

          case class Self(href: String)

          object Self {
            implicit val e: Encoder[Self] = semiauto.deriveEncoder
            implicit val d: Decoder[Self] = semiauto.deriveDecoder
          }

        }

      }

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

      case class Links(first: Links.First, self: Links.Self, next: Option[Links.Next], last: Links.Last)

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
        )

        object SchoolUnitRep {

          implicit val e: Encoder[SchoolUnitRep] = semiauto.deriveEncoder
          implicit val d: Decoder[SchoolUnitRep] = semiauto.deriveDecoder

          case class Link(self: Link.Self, statistics: Link.Statistics)

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
