import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val scalaTools = "scalaTools" at "http://scala-tools.org/repo/"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7.1" % "test"
  
  override def includeTest(s: String) = s endsWith "Specs"
}
