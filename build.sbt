ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

val catsCore   = "org.typelevel" %% "cats-core"   % "2.7.0"
val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.13"

lazy val root = (project in file("."))
  .settings(
    name := "scala-with-cats"
  )
  .settings(
    libraryDependencies ++= Seq(
      catsCore,
      catsEffect
    )
  )
