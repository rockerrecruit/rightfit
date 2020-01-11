name := "rightfit"

version := "0.1"

scalaVersion := "2.13.1"

val ZioVersion = "1.0.0-RC17"

libraryDependencies ++= List(
  "dev.zio" %% "zio" % ZioVersion,
  "dev.zio" %% "zio-streams" % ZioVersion
)

