package com.rightfit.api.skolverket

import com.rightfit.api.BlazeHttpClient
import com.rightfit.api.skolverket.Api.SchoolUnitSummary.Body.Embedded.SchoolUnitRep
import com.rightfit.api.skolverket.Api.{GymnasiumDetailedUnit, PotentialSchools}
import org.http4s._
import org.http4s.client._
import zio._
import zio.interop.catz._
import cats.syntax.show._

trait SkolverketClient[R] {
  def service(blazeClient: Client[Task]): SkolverketClient.Service[R]
}

object SkolverketClient {

  trait Service[R] {

    def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]]

    def getSchoolsByPage(blazeClient: Client[Task], upToPage: Int): Task[List[Api.SchoolUnitSummary]] = {
      val maxPage = if (upToPage > 14) 14 else upToPage
      val requests = List.range(0, maxPage).map { p =>
        val e1 = Uri.unsafeFromString(
          s"https://api.skolverket.se/planned-educations/school-units?page=$p&size=100&typeOfSchooling=gy"
        )
        Request[Task](
          Method.GET,
          e1,
          headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
        )
      }
      ZIO.foreachParN(15)(requests)(req => blazeClient.expect[Api.SchoolUnitSummary](req))
    }

    def uriFromUnit(unit: String): Uri =
      Uri.unsafeFromString(s"https://api.skolverket.se/planned-educations/school-units/$unit/statistics/gy")

    def requestFromUri(uri: Uri): Request[Task] =
      Request[Task](
        Method.GET,
        uri,
        headers = Headers.of(Header("Accept", s"application/vnd.skolverket.plannededucations.api.v2.hal+json"))
      )

  }

  final class Test[R] extends SkolverketClient[R] {
    override def service(blazeClient: Client[Task]): Service[R] = new Service[R] {
      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          schoolSummaries <- getSchoolsByPage(blazeClient, upToPage = 1)
          gymnasiumUnits  <- ZIO.foreach(schoolSummaries) { summary =>
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
    override def service(blazeClient: Client[Task]): Service[R] = new Service[R] {

      override def retrieveAllSchoolsWithStats: Task[List[(SchoolUnitRep, GymnasiumDetailedUnit)]] = {
        for {
          schoolSummaries <- getSchoolsByPage(blazeClient, upToPage = 14)
          gymnasiumUnits  <- ZIO.foreach(schoolSummaries) { summary =>
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
      c <- BlazeHttpClient.client
      _ <- for {
            potenialSchools <- c.use { v =>
                                 val service = new Test[Any].service(v)
                                 service.retrieveAllSchoolsWithStats.map(getSchoolByGrade(_, avgGrade = 240.0))
                               }
            _               <- zio.console.putStrLn(potenialSchools.show)
          } yield ()
    } yield 0).catchAllCause(cause => zio.console.putStrLn(cause.prettyPrint).as(1))
  }

}
