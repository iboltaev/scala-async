name := "scala-async"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "com.twitter" %% "finagle-httpx" % "6.27.0",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta9",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta9",
  "org.scalaz" %% "scalaz-core" % "7.2.0")
