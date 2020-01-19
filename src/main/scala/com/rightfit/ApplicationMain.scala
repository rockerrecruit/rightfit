package com.rightfit
import cats.effect.ExitCode
import com.rightfit.api.{Api, Configuration}
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.putStrLn
import zio.interop.catz._

object ApplicationMain extends App {

  type AppEnvironment = Clock with Blocking with zio.console.Console

  type AppTask[A] = RIO[AppEnvironment, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program: ZIO[ZEnv, Throwable, Unit] = for {
      conf    <- api.loadConfig.provide(Configuration.Live)
      httpApp  = Router[AppTask](mappings = "/score" -> Api(s"${conf.api.endpoint}/score").route).orNotFound
      server   = ZIO.runtime[AppEnvironment].flatMap { implicit rts =>
                   BlazeServerBuilder[AppTask]
                     .bindHttp(conf.api.port, host = "0.0.0.0")
                     .withHttpApp(CORS(httpApp))
                     .serve
                     .compile[AppTask, AppTask, ExitCode]
                     .drain
                 }
      _        <- zio.console.putStrLn(line = "Starting Server...")
      program  <- server
    } yield program

    program.foldM(
      err => putStrLn(line = s"Execution failed with: $err") *> IO.succeed(1),
      _   => IO.succeed(0)
    )
  }
}
