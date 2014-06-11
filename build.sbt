import scalariform.formatter.preferences._

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
)

organization  := "com.traversalsoftware"

name := "csveer"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0" cross CrossVersion.full,
  "org.scalatest" % "scalatest_2.10" % "2.1.5" % "test",
  "org.specs2" %%  "specs2-core"   % "2.3.7" % "test",
  "com.typesafe.play" %% "play" % "2.2.1"
)

scalacOptions ++= Seq("-feature")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignParameters, true)
  .setPreference(RewriteArrowSymbols, true)