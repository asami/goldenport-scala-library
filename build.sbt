organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "0.1.2"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "org.goldenport" %% "scalatestlib" % "0.1.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
