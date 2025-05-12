organization := "org.goldenport"

name := "goldenport-scala-lib"

version := "2.2.2"

scalaVersion := "2.12.18"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// incOptions := incOptions.value.withNameHashing(true)

javacOptions ++= Seq("--release", "21")

javaOptions ++= Seq(
  "--module-path", sys.props("java.class.path"),
  "--add-modules", "javafx.controls,javafx.fxml"
)

lazy val javafxVersion = "21"

lazy val osClassifier = System.getProperty("os.name").toLowerCase match {
  case name if name.contains("mac")   => "mac"
  case name if name.contains("win")   => "win"
  case name if name.contains("linux") => "linux"
  case _ => throw new RuntimeException("Unsupported OS")
}

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2021-scala2.12/releases"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// For WPath and GXml. TODO separate to reduce dependencies.
libraryDependencies ++= {
  val v = scalaVersion.value
  if (v.startsWith("2.10"))
    Nil
  else
    Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
    )
}

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.4" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-kestrel" % "2.1.1"

// Defines important library dependencies

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a" exclude("org.scala-lang", "scala-library_2.10.5")

libraryDependencies += "com.madgag" %% "scala-io-core" % "0.4.9"

libraryDependencies += "com.madgag" %% "scala-io-file" % "0.4.9"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.20.0"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "io.circe" %% "circe-core" % "0.14.13"

libraryDependencies += "io.circe" %% "circe-generic" % "0.14.13"

libraryDependencies += "io.circe" %% "circe-parser" % "0.14.13"

libraryDependencies += "io.circe" %% "circe-yaml" % "1.15.0"

libraryDependencies += "org.yaml" % "snakeyaml" % "2.4"

// libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.9.4" 

libraryDependencies += "org.typelevel" %% "spire" % "0.17.0"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "provided"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10" % "compile"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "provided"

libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.2"

libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.22" % "compile"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2" % "compile"

libraryDependencies += "black.ninia" % "jep" % "3.9.0" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "5.10.0.202012080955-r" % "provided"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies ++= Seq(
  "org.openjfx" % "javafx-base" % javafxVersion classifier osClassifier,
  "org.openjfx" % "javafx-controls" % javafxVersion classifier osClassifier,
  "org.openjfx" % "javafx-graphics" % javafxVersion classifier osClassifier,
  "org.openjfx" % "javafx-fxml" % javafxVersion classifier osClassifier,
  "org.openjfx" % "javafx-swing" % javafxVersion classifier osClassifier,
  "org.openjfx" % "javafx-web" % javafxVersion classifier osClassifier
)

// libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.0.0" % "test"

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
