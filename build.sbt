name := "rightfit"

version := "0.1"

scalaVersion := "2.13.1"

val ZioVersion = "1.0.0-RC17"

val Http4sVersion          = "0.21.0-M6"
val DoobieVersion          = "0.8.8"
val ZIOVersion             = "1.0.0-RC17"
val PureConfigVersion      = "0.12.2"
val H2Version              = "1.4.199"
val CirceVersion           = "0.12.2"
val CirceDerivationVersion = "0.12.0-M7"

libraryDependencies ++= List(
  "dev.zio"               %% "zio"                  % ZioVersion,
  "dev.zio"               %% "zio-streams"          % ZioVersion,
  "dev.zio"               %% "zio-interop-cats"     % "2.0.0.0-RC10",
  "org.http4s"            %% "http4s-blaze-server"  % Http4sVersion,
  "org.http4s"            %% "http4s-blaze-client"  % Http4sVersion,
  "org.http4s"            %% "http4s-circe"         % Http4sVersion,
  "org.http4s"            %% "http4s-dsl"           % Http4sVersion,
  "org.tpolecat"          %% "doobie-core"          % DoobieVersion,
  "org.tpolecat"          %% "doobie-h2"            % DoobieVersion,
  "io.circe"              %% "circe-generic"        % CirceVersion,
  "io.circe"              %% "circe-generic-extras" % CirceVersion,
  "com.github.pureconfig" %% "pureconfig"           % PureConfigVersion,
  "com.h2database"        % "h2"                    % H2Version,
  "org.slf4j"             % "slf4j-log4j12"         % "1.7.26"
)

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl"          % Http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % Http4sVersion
)

addCommandAlias("runSkolverketApi", "runMain com.rightfit.api.TestBlazeHttpClient")
