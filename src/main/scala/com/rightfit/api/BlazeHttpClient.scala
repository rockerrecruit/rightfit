package com.rightfit.api

import cats.effect.Resource
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import zio.interop.catz._
import zio.{Runtime, Task, ZIO}

object BlazeHttpClient {
  def client: ZIO[Any, Nothing, Resource[Task, Client[Task]]] =
    ZIO.runtime.map { implicit runtime: Runtime[Any] =>
      BlazeClientBuilder[Task](runtime.platform.executor.asEC).resource
    }
}
