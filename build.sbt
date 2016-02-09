name := "CustomSlickGenerator"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick-codegen" % "3.1.0",
  "com.typesafe.slick" %% "slick" % "3.1.0",
  "com.typesafe.play" %% "play-json" % "2.4.4",
  "mysql" % "mysql-connector-java" % "5.1.37",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0",
  "com.github.tototoshi" % "slick-joda-mapper_2.11" % "2.1.0",
  "joda-time" % "joda-time" % "2.9.1"
)