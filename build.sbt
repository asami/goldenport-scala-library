organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "1.4.8"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// scalaz-stream
// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.4" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-kestrel" % "1.0.0"

// Defines important library dependencies

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.6a" exclude("org.scala-lang", "scala-library_2.10.4")

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "1.6.0"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "io.circe" %% "circe-core" % "0.8.0"

libraryDependencies += "io.circe" %% "circe-generic" % "0.8.0"

libraryDependencies += "io.circe" %% "circe-parser" % "0.8.0"

libraryDependencies += "io.circe" %% "circe-yaml" % "0.8.0"

libraryDependencies += "org.yaml" % "snakeyaml" % "2.4"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4" 

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "provided"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10" % "compile"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "provided"

libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.6"

libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.22" % "compile"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2" % "compile"

libraryDependencies += "black.ninia" % "jep" % "3.9.0" % "compile"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "5.10.0.202012080955-r" % "provided"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.6"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.0.0" % "test"

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
