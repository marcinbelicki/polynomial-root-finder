lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "pl.belicki",
      scalaVersion := "2.13.12"
    )),
    name := "polynomial-root-finder"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
