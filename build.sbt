name := "PEPM19-poster-scala"

version := "1.0"

organization := "tdauth"

scalaVersion := "2.12.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("tdauth.pepm19.HolidayBooking")
// set the main class for packaging the main jar
mainClass in (Compile, packageBin) := Some("tdauth.pepm19.HolidayBooking")