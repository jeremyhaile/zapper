import sbt._

class ScalatraAkkaTemplate(info: ProjectInfo) extends DefaultWebProject(info) with AkkaProject {

  // scalatra
  val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  val sonatypeNexusReleases = "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases"
  val scalatra = "org.scalatra" %% "scalatra" % "2.0.0-SNAPSHOT"
 
  // jetty
  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.22" % "test"
  val servletApi = "javax.servlet" % "servlet-api" % "2.5" % "provided"
}
