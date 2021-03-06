import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.1.0-M5"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "none",
    version := "1.0.0",
    scalacOptions ++= Seq("-deprecation"),
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  //
  // 
  //
  val cleanOutputData = taskKey[Unit]("cleanTemp")

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      scalacOptions := Seq("-deprecation"),
      run <<= run in Compile in test,
      compile in Compile <<= (compile in Compile).dependsOn(Def.task { 
        // somehow this doesn't get executed here, only in a subproject
        println("About to extract call graph while compiling...") 
      }),
      cleanOutputData := {
        println("Running task that does nothing...")
      }
    )
  ) aggregate(grapher)

  lazy val util: Project = Project(
    "util",
    file("util"),
    settings = buildSettings ++ Seq(
        libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12", scalacOptions := Seq("-deprecation"),
        libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.1",
        libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2",
        name         := "util",
        organization := "articlio",
        version      := "0.1-SNAPSHOT"
      )
  ) 

  lazy val defmacro: Project = Project(
    "defmacro", 
    file("defmacro"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.1",
      libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"
    )
  ) dependsOn(util)

  lazy val grapher: Project = Project(
    "grapher",
    file("grapher"),
    settings = buildSettings ++ Seq(
      scalacOptions := Seq("-deprecation"),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      ),
      libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.1"
    )
  ) dependsOn(util, defmacro)

  lazy val test: Project = Project(
    "test",
    file("test"),
    settings = buildSettings ++ Seq(
        libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12", scalacOptions := Seq("-deprecation"),
        compile in Compile <<= (compile in Compile).dependsOn(Def.task { 
          println("\nAbout to extract call graph while compiling...\n") 
        })
      )
) dependsOn(grapher)
}
