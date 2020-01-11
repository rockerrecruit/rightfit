package com.rightfit.api
import pureconfig._

case class Config(api: ApiConfig, dbConfig: DbConfig)
case class ApiConfig(endpoint: String, port: Int)
case class DbConfig(url: String, user: String, password: String)

import zio.{RIO, Task}

trait Configuration extends Serializable {
  val config: Configuration.Service[Any]
}

object Configuration {

  trait Service[R] {
    val load: RIO[R, Config]
  }

  trait Live extends Configuration {

    val config: Service[Any] = new Service[Any] {
      import pureconfig.generic.auto._

      val load: Task[Config] = Task.effect(ConfigSource.default.loadOrThrow[Config])
    }
  }

  object Live extends Live

  trait Test extends Configuration {

    val config: Service[Any] = new Service[Any] {

      val load: Task[Config] = Task.effectTotal(
        Config(ApiConfig("loacalhost", 8080), DbConfig("localhost", "", ""))
      )
    }
  }
}
