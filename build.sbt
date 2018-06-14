organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "1.2.22"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.3" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-kestrel" % "1.0.0"

// Defines important library dependencies

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.6a" exclude("org.scala-lang", "scala-library_2.10.4")

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "1.6.0"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4" 

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.18" % "compile"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
