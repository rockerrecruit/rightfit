package com.rightfit.api

import cats.effect.Resource
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import zio.interop.catz._
import zio.{Runtime, Task, ZIO}

trait BlazeHttpClient {
  def blazeClient: BlazeHttpClient.Service[Any]
}

object BlazeHttpClient {

  trait Service[R] {
    def httpClient: ZIO[Any, Nothing, Resource[Task, Client[Task]]]
  }

  trait Live extends BlazeHttpClient {
    override def blazeClient: BlazeHttpClient.Service[Any] = new Service[Any] {
      override def httpClient: ZIO[Any, Nothing, Resource[Task, Client[Task]]] = {
        ZIO.runtime.map { implicit runtime: Runtime[Any] =>
          BlazeClientBuilder[Task](runtime.platform.executor.asEC).resource
        }
      }
    }

  }

  object Live extends Live
}
