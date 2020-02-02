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

trait SkolverketClient {
  def skolverketClient: SkolverketClient.Service[Any]
}

object SkolverketClient {

  trait Service[R] {
    def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]]
  }

  trait Live extends BlazeHttpClient with SkolverketClient {
    override def skolverketClient: Service[Any] = new Service[Any] {
      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          cr      <- blazeClient.httpClient
          schools <- cr.use(c => getSchoolsWithStats(c, upToPage = 14))
        } yield schools
      }
    }
  }

  trait Test extends BlazeHttpClient with SkolverketClient {
    override def skolverketClient: Service[Any] = new Service[Any] {
      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          cr      <- blazeClient.httpClient
          schools <- cr.use(c => getSchoolsWithStats(c, upToPage = 1))
        } yield schools
      }
    }
  }

  object Helpers {

    def uriFromUnit(unit: String): Uri =
      Uri.unsafeFromString(s"https://api.skolverket.se/planned-educations/school-units/$unit/statistics/gy")

    def requestFromUri(uri: Uri): Request[Task] =
      Request[Task](
        Method.GET,
        uri,
        headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
      )

    def getSchoolsByPage(client: Client[Task], upToPage: Int): Task[List[Api.SchoolUnitSummary]] = {
      val maxPage = if (upToPage > 14) 14 else upToPage
      val requests = List.range(0, maxPage).map { p =>
        val strUrl  = s"https://api.skolverket.se/planned-educations/school-units?page=$p&size=100&typeOfSchooling=gy"
        val uri     = Uri.unsafeFromString(strUrl)
        val headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        Request[Task](Method.GET, uri, headers = headers)
      }
      ZIO.foreachParN(15)(requests)(req => client.expect[Api.SchoolUnitSummary](req))
    }

    def getSchoolsWithStats(client: Client[Task], upToPage: Int): Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
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
              summary <- client.expect[Api.SchoolUnitSummary](req)
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

  def getSchoolByGrade(schools: List[(SchoolUnitRep, GymnasiumDetailedUnit)], avgGrade: Double): PotentialSchools = {
    val filteredSchools = schools
      .map { case (rep, school) => school.toGymnasiumUnit(rep) }
      .filter(_.isWithin10Avg(avgGrade))
    PotentialSchools(filteredSchools)
  }

}

object TestBlazeHttpClient extends App {
  import SkolverketClient._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    (for {
      _              <- zio.console.putStrLn(line = s"Running Test Client...")
      avgGrade        = 240.0
      service         = new BlazeHttpClient.Live with SkolverketClient.Test {}
      schoolDetails  <-  service.skolverketClient.retrieveAllSchoolsWithStats
      schools         = getSchoolByGrade(schoolDetails, avgGrade)
      _              <-  zio.console.putStrLn(schools.show)
    } yield 0).catchAllCause(cause => zio.console.putStrLn(cause.prettyPrint).as(1))
  }

}
