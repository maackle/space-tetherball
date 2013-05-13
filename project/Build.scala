import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appName         = "tetherball"
  val appVersion      = "0.1a"

  val skitch = Project("skitch", file("skitch"), settings = Project.defaultSettings ++ Seq(

  ))

  val test = Project("tetherball", file("tetherball"), settings = Project.defaultSettings ++ Seq(

  )) dependsOn(skitch) aggregate(skitch)


}
