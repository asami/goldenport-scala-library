organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "2.1.7"

scalaVersion := "2.12.7"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// For WPath and GXml. TODO separate to reduce dependencies.
libraryDependencies <++= scalaVersion { v =>
  if (v.startsWith("2.10"))
    Nil
  else
    Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "org.scala-lang.modules" %% "scala-xml" %  "1.1.1"
    )
}

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.4" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-kestrel" % "2.1.0"

// Defines important library dependencies

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a" exclude("org.scala-lang", "scala-library_2.10.5")

libraryDependencies += "com.madgag" %% "scala-io-core" % "0.4.9"

libraryDependencies += "com.madgag" %% "scala-io-file" % "0.4.9"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.20.0"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4" 

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "provided"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10" % "compile"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "provided"

libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.22" % "compile"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2" % "compile"

libraryDependencies += "black.ninia" % "jep" % "3.9.0" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.0.0" % "test"

//
// publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
// AutoMkcol.globalSettings

// credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

// publishTo <<= version { v: String =>
//   val backlog = "https://everforth.backlog.jp/dav/APC/maven/"
//   if (v.trim.endsWith("SNAPSHOT"))
//     Some("Backlog snapshots" at backlog + "snapshots")
//   else
//     Some("Backlog releases" at backlog + "releases")
// }

val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}
