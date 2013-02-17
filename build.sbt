organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "0.1.7-SNAPSHOT"

// scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "0.2.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
