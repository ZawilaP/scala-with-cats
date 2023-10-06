name := "scala-with-cats"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "2.9.0"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)