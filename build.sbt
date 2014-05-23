resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

// For Scala 2.11.0
scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0"
)