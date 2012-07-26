
name:="termware"

organization:="com.github.termware"

scalaVersion := "2.10.0-M5"

scalacOptions ++= Seq("-unchecked","-deprecation")

version:="0.0.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0-M5"

// hack to support scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "1.9-2.10.0-M5-B2" % "test"



publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://rssh.github.com/stermware</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>pt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:rssh/stermware.git</url>
    <connection>scm:git:git@github.com:rssh/stermware.git</connection>
  </scm>
  <developers>
    <developer>
      <id>rssh</id>
      <name>Ruslan Shevchenko</name>
      <url>https://github.com/rssh</url>
    </developer>
  </developers>
)

