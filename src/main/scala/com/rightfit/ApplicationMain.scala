package com.rightfit
import cats.effect.ExitCode
import com.rightfit
import com.rightfit.api.{BlazeHttpClient, Configuration, Server}
import com.rightfit.api.skolverket.SkolverketClient._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import org.slf4j.LoggerFactory
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.putStrLn
import zio.interop.catz._

object ApplicationMain extends App {

  type AppEnvironment = Clock with Blocking with zio.console.Console
  type AppTask[A]     = RIO[AppEnvironment, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val log = LoggerFactory.getLogger(this.getClass)
    val program: ZIO[ZEnv, Throwable, Unit] = for {
      conf    <- api.loadConfig.provide(Configuration.Live)
      _       <- ZIO.effect(log.debug(s"Starting Server. Retrieving schools from Skolverket..."))
      client  <- BlazeHttpClient.client
      schools <- client.use { c =>
                  new Live[Any].service(c).retrieveAllSchoolsWithStats
                 }
      _       <- ZIO.effect(log.debug(s"Retrieved ${schools.size} schools."))
      httpApp  = Router[AppTask](mappings = "/score" -> Server(s"${conf.api.endpoint}/score").route(schools)).orNotFound
      _       <- ZIO.runtime[AppEnvironment].flatMap { implicit rts =>
                   BlazeServerBuilder[AppTask]
                     .bindHttp(conf.api.port, host = "0.0.0.0")
                     .withHttpApp(CORS(httpApp))
                     .serve
                     .compile[AppTask, AppTask, ExitCode]
                     .drain
                 }
    } yield ()

    program.foldM(
      err => putStrLn(line = s"Execution failed with: $err") *> IO.succeed(1),
      _   => IO.succeed(0)
    )
  }
}
