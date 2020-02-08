package com.rightfit
import cats.effect.ExitCode
import com.rightfit.api.skolverket.SkolverketClient
import com.rightfit.api.{BlazeHttpClient, Configuration, Server}
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import org.slf4j.{Logger, LoggerFactory}
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.putStrLn
import zio.interop.catz._
import zio.console.Console

object ApplicationMain extends App {

  type AppEnvironment = Clock with Blocking with zio.console.Console with BlazeHttpClient.Live
  type AppTask[A]     = RIO[AppEnvironment, A]

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def server: ZIO[AppEnvironment, Throwable, Unit] = for {
    conf    <- api.loadConfig.provide(Configuration.Live)
    _       <- ZIO.effect(log.debug(s"Starting Server. Retrieving schools from Skolverket..."))
    client  = SkolverketClient.Live
    schools <- client.skolverketClient.retrieveAllSchoolsWithStats
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

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val env = new Clock.Live with Blocking.Live with Console.Live with BlazeHttpClient.Live
    server.provide(env).foldM(
      err => putStrLn(line = s"Execution failed with: $err") *> IO.succeed(1),
      _   => IO.succeed(0)
    )
  }
  
}
