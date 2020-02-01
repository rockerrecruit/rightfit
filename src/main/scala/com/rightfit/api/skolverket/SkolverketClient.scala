package com.rightfit.api.skolverket

import com.rightfit.api.BlazeHttpClient
import com.rightfit.api.skolverket.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import com.rightfit.api.skolverket.Api.{GymnasiumDetailedUnit, PotentialSchools, SchoolUnitSummary}
import org.http4s._
import org.http4s.client._
import zio._
import zio.interop.catz._
import cats.syntax.show._

trait SkolverketClient[R] {
  def service(blazeClient: Client[Task]): SkolverketClient.Service[R]
}

object SkolverketClient {

  abstract class Service[R](client: Client[Task]) {

    def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]]

    def uriFromUnit(unit: String): Uri =
      Uri.unsafeFromString(s"https://api.skolverket.se/planned-educations/school-units/$unit/statistics/gy")

    def requestFromUri(uri: Uri): Request[Task] =
      Request[Task](
        Method.GET,
        uri,
        headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
      )

    def getSchoolsByPage(upToPage: Int): Task[List[Api.SchoolUnitSummary]] = {
      val maxPage  = if (upToPage > 14) 14 else upToPage
      val requests = List.range(0, maxPage).map { p =>
        val strUrl  = s"https://api.skolverket.se/planned-educations/school-units?page=$p&size=100&typeOfSchooling=gy"
        val uri     = Uri.unsafeFromString(strUrl)
        val headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        Request[Task](Method.GET, uri, headers = headers)
      }
      ZIO.foreachParN(15)(requests)(req => client.expect[Api.SchoolUnitSummary](req))
    }

    //FIXME: A GET on the 'nextLink' returns a 302. Perhaps I'm supposed to use a different header?
    def getSchoolsByLink(maybeLink: Option[SchoolUnitSummary.Body.Links.Next]): Task[List[Api.SchoolUnitSummary]] = {
      def recurse(maybeLink: Option[SchoolUnitSummary.Body.Links.Next]): Task[List[Api.SchoolUnitSummary]] = {
        maybeLink match {
          case Some(link) =>
            val uri     = Uri.unsafeFromString(link.href)
            val headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
            val req     = Request[Task](Method.GET, uri, headers = headers)
            for {
              summary <-  client.expect[Api.SchoolUnitSummary](req)
              _       <-  ZIO.effect(println(s"Successfully retrieved summary..."))
              result  <-  summary.body._links.next match {
                case Some(nextLink) =>
                  ZIO.effect(println(s"Got nextlink: $nextLink")) *>
                  recurse(Some(nextLink)).map(s => s.appended(summary))
                case None =>
                  Task(List[Api.SchoolUnitSummary]())
              }
            } yield result
          case None =>
            Task(List[Api.SchoolUnitSummary]())
        }
      }
      val startUrl  = "https://api.skolverket.se/planned-educations/school-units?size=100&typeOfSchooling=gy"
      val startLink = SchoolUnitSummary.Body.Links.Next(startUrl)
      recurse(Some(startLink))
    }
  }

  final class Test[R] extends SkolverketClient[R] {
    override def service(blazeClient: Client[Task]): Service[R] = new Service[R](blazeClient) {
      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          schoolSummaries <- getSchoolsByPage(upToPage = 1)
          gymnasiumUnits  <- ZIO.foreachParN(15)(schoolSummaries) { summary =>
                               ZIO.foreachParN(15)(summary.body._embedded.listedSchoolUnits) { u =>
                                 blazeClient
                                   .expect[GymnasiumDetailedUnit](requestFromUri(uriFromUnit(u.code)))
                                   .fold(_ => Map.empty[SchoolUnitRep, GymnasiumDetailedUnit],
                                     unit => Map[SchoolUnitRep, GymnasiumDetailedUnit](u -> unit))
                               }.map(_.flatten)
                             }.map(_.flatten)
        } yield gymnasiumUnits
      }
    }
  }

  final class Live[R] extends SkolverketClient[R] {
    override def service(blazeClient: Client[Task]): Service[R] = new Service[R](blazeClient) {
      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          schoolSummaries <- getSchoolsByPage(upToPage = 14)
          gymnasiumUnits  <- ZIO.foreachParN(5)(schoolSummaries) { summary =>
                               ZIO.foreachParN(5)(summary.body._embedded.listedSchoolUnits) { u =>
                                 blazeClient
                                   .expect[GymnasiumDetailedUnit](requestFromUri(uriFromUnit(u.code)))
                                   .fold(_ => Map.empty[SchoolUnitRep, GymnasiumDetailedUnit],
                                     unit => Map[SchoolUnitRep, GymnasiumDetailedUnit](u -> unit))
                               }.map(_.flatten)
                             }.map(_.flatten)
        } yield gymnasiumUnits
      }
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
      c       <- BlazeHttpClient.client
      s        = new Test[Any].service _
      _       <- for {
                   schools <- c.use(v => s(v).retrieveAllSchoolsWithStats.map(getSchoolByGrade(_, avgGrade = 240.0)))
                   _       <- zio.console.putStrLn(schools.show)
                 } yield ()
    } yield 0).catchAllCause(cause => zio.console.putStrLn(cause.prettyPrint).as(1))
  }

}
