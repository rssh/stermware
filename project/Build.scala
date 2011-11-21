import sbt._
import Keys._

import java.io.File;

object TermWareBuild extends Build
{

  override def projects = Seq(root)

  lazy val root = Project("root", file("."),
                      settings = Defaults.defaultSettings ++
                                 rootSettings);

  lazy val rootSettings = Seq(
     name:="termware",
     organization:="ua.gradsoft",
     version:="0.5.0",
     scalaVersion:="2.9.1",
     scalacOptions ++= Seq("-unchecked", "-deprecation")
     libraryDependencies += ("org.scalatest" %% "scalatest" % "1.6.1" % "test")
   );

}
