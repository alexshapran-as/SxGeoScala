name := "API-ip"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.23",
  "com.typesafe.akka" %% "akka-http"   % "10.1.8",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.9",
  "com.typesafe.akka" %% "akka-stream" % "2.5.23",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.23" % Test,
  "com.enragedginger" %% "akka-quartz-scheduler" % "1.8.1-akka-2.5.x",
  "org.mongodb.scala" %% "mongo-scala-driver" % "2.6.0",
  "org.json4s" %% "json4s-native" % "3.6.7",
  "joda-time" % "joda-time" % "2.10.3",
  "org.json4s" %% "json4s-jackson" % "3.6.7",
  "org.apache.logging.log4j" % "log4j-api" % "2.11.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.11.1",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.23",
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")