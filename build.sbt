organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "2.0.1"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// For WPath and GXml. TODO separate to reduce dependencies.
libraryDependencies <++= scalaVersion { v =>
  if (v.startsWith("2.10"))
    Nil
  else
    Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scala-lang.modules" %% "scala-xml" %  "1.0.4"
    )
}

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.3" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-kestrel" % "2.0.0"

// Defines important library dependencies

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.6a" exclude("org.scala-lang", "scala-library_2.10.5")

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.0.0"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4"

// libraryDependencies += "org.scalikejdbc" %% "scalikejdbc" % "2.2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.0.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
