package com.rightfit.api.skolverket

import com.rightfit.api.BlazeHttpClient
import com.rightfit.api.skolverket.Api._
import com.rightfit.api.skolverket.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import org.http4s._
import org.http4s.client._
import zio._
import zio.interop.catz._
import SkolverketClient.Helpers._
import cats.syntax.show._
import com.rightfit.model.{AverageGrade, County, ProgramType}
import org.slf4j.{Logger, LoggerFactory}
import zio.console.Console

trait SkolverketClient {
  def skolverketClient: SkolverketClient.Service[BlazeHttpClient]
}

object SkolverketClient {

  type SchoolDetails = List[(SchoolUnitRep, GymnasiumDetailedUnit)]

  trait Service[R] {
    def retrieveAllSchoolsWithStats: ZIO[BlazeHttpClient, Throwable, List[(SchoolUnitRep, GymnasiumDetailedUnit)]]
  }

  trait Live extends SkolverketClient {
    override def skolverketClient: Service[BlazeHttpClient] = new Service[BlazeHttpClient] {
      override def retrieveAllSchoolsWithStats: ZIO[BlazeHttpClient, Throwable, SchoolDetails] = {
        for {
          blazeEnv <- ZIO.environment[BlazeHttpClient]
          cr       <- blazeEnv.blazeHttpClient.resource
          schools  <- cr.use(c => getSchoolsWithStats(c, upToPage = 14))
        } yield schools
      }
    }
  }

  object Live extends Live

  trait Test extends SkolverketClient {
    override def skolverketClient: Service[BlazeHttpClient] = new Service[BlazeHttpClient] {
      override def retrieveAllSchoolsWithStats: ZIO[BlazeHttpClient, Throwable, SchoolDetails] = {
        for {
          blazeEnv <- ZIO.environment[BlazeHttpClient]
          cr       <- blazeEnv.blazeHttpClient.resource
          schools  <- cr.use(c => getSchoolsWithStats(c, upToPage = 1))
        } yield schools
      }
    }
  }

  object Test extends Test

  object Helpers {

    def uriFromUnit(unit: String): Uri =
      Uri.unsafeFromString(s"https://api.skolverket.se/planned-educations/school-units/$unit/statistics/gy")

    def requestFromUri(uri: Uri): Request[Task] =
      Request[Task](
        Method.GET,
        uri,
        headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
      )

    def getSchoolsByPage(client: Client[Task], upToPage: Int): Task[List[SchoolUnitSummary]] = {
      val maxPage = if (upToPage > 14) 14 else upToPage
      val requests = List.range(0, maxPage).map { p =>
        val strUrl  = s"https://api.skolverket.se/planned-educations/school-units?page=$p&size=100&typeOfSchooling=gy"
        val uri     = Uri.unsafeFromString(strUrl)
        val headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        Request[Task](Method.GET, uri, headers = headers)
      }
      ZIO.foreachParN(15)(requests)(req => client.expect[SchoolUnitSummary](req))
    }

    def getSchoolsWithStats(client: Client[Task], upToPage: Int): Task[SchoolDetails] = {
      for {
        schoolSummaries <- getSchoolsByPage(client, upToPage)
        gymnasiumUnits  <- ZIO.foreachParN(5)(schoolSummaries) { summary =>
                             ZIO.foreachParN(5)(summary.body._embedded.listedSchoolUnits) { u =>
                               client
                                 .expect[GymnasiumDetailedUnit](requestFromUri(uriFromUnit(u.code)))
                                 .fold(_ => Map.empty, unit => Map(u -> unit))
                             }.map(_.flatten)
                           }.map(_.flatten)
      } yield gymnasiumUnits
    }

    //FIXME: A GET on the 'nextLink' returns a 302. Perhaps I'm supposed to use a different header?
    def getSchoolsByLink(
      client: Client[Task],
      maybeLink: Option[SchoolUnitSummary.Body.Links.Next]
    ): Task[List[SchoolUnitSummary]] = {

      def recurse(maybeLink: Option[SchoolUnitSummary.Body.Links.Next]): Task[List[SchoolUnitSummary]] = {
        maybeLink match {
          case Some(link) =>
            val uri    = Uri.unsafeFromString(link.href)
            val header = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
            val req    = Request[Task](Method.GET, uri, headers = header)
            for {
              summary <- client.expect[SchoolUnitSummary](req)
              result  <- summary.body._links.next
                          .fold(Task(List.empty[SchoolUnitSummary]))(nl => recurse(Some(nl)).map(_.appended(summary)))
            } yield result
          case None =>
            Task(List.empty)
        }
      }

      val startUrl  = "https://api.skolverket.se/planned-educations/school-units?size=100&typeOfSchooling=gy"
      val startLink = SchoolUnitSummary.Body.Links.Next(startUrl)
      recurse(Some(startLink))
    }

  }

  def getSchoolByGrade(
    schoolDetails: SchoolDetails,
    avgGrade: AverageGrade,
    county: County,
    programType: ProgramType
  ): PotentialSchools = {
    val filteredSchools = schoolDetails
      .map { case (rep, school) => school.toGymnasiumUnit(rep) }
      .filter(unit => unit.isRelevant(avgGrade, county, programType))
    PotentialSchools(filteredSchools)
  }

}

object TestSkolverketClient extends App {
  import SkolverketClient._

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def testClient: ZIO[Console with BlazeHttpClient, Throwable, Unit] = {
    val avgGrade = AverageGrade(240.0)
    val service  = SkolverketClient.Test
    for {
      _             <- ZIO.effect(log.debug( s"Retrieving a few schools with avg grade around ${avgGrade.show}"))
      schoolDetails <- service.skolverketClient.retrieveAllSchoolsWithStats
      schools        = getSchoolByGrade(schoolDetails, avgGrade, County("01"), ProgramType("NA"))
      _             <- ZIO.effect(log.debug(schools.show))
      _             <- ZIO.effect(log.debug(s"$schools"))
    } yield ()
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    testClient.provide(new zio.console.Console.Live with BlazeHttpClient.Live)
      .map(_ => 0)
      .catchAllCause(cause => zio.console.putStrLn(cause.prettyPrint).as(1))
  }

}
