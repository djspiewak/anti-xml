import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info)  with IdeaProject {

  val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test" withSources
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7.1" % "test" withSources
  val jvmSizeOf = "com.github.dmlap" %% "sizeof" % "0.1" % "test" from "http://cloud.github.com/downloads/dmlap/jvm-sizeof/jvm-sizeof-0.1.jar"

  /* Performance Tests */
  val perfDependencies = testClasspath +++ testDependencies.all
  val sizeOfJar = testDependencies.libraries ** (jvmSizeOf.name + "*.jar")
  lazy val testPerf = task {
    log.info("Running performance tests...")
    new Fork.ForkScala("com.codecommit.antixml.Performance")(None, Seq("-javaagent:" + sizeOfJar.absString), perfDependencies.getFiles, Seq(), log) match {
      case 0 => None
      case x => Some("failed with error code " + x)
    }
  } dependsOn (testCompile, copyTestResources)
}
