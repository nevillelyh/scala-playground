name := "scala-playground"
description := "Scala Playground"

organization := "me.lyh"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-target:jvm-1.8", "-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)
