name := "scala-playground"
description := "Scala Playground"

organization := "me.lyh"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-target:jvm-1.8", "-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.twitter" %% "algebird-core" % "0.13.0",
  "org.scalanlp" %% "breeze" % "0.13.1",
  "org.typelevel" %% "cats" % "0.9.0"
)
