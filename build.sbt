name := "rightfit"

version := "0.1"

scalaVersion := "2.13.1"

val ZioVersion = "1.0.0-RC17"

val Http4sVersion = "0.21.0-M6"
val DoobieVersion = "0.8.8"

libraryDependencies ++= List(
  "dev.zio" %% "zio" % ZioVersion,
  "dev.zio" %% "zio-streams" % ZioVersion,
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-circe" % Http4sVersion,
  "org.http4s" %% "http4s-dsl" % Http4sVersion,
  "org.tpolecat" %% "doobie-core" % DoobieVersion,
  "org.tpolecat" %% "doobie-h2" % DoobieVersion
)

