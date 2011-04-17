import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {

  val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test" withSources
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7.1" % "test" withSources
  val jvmSizeOf = "com.github.dmlap" %% "sizeof" % "0.1" % "test" from "http://cloud.github.com/downloads/dmlap/jvm-sizeof/jvm-sizeof-0.1.jar"

  /* Performance Tests */
  val perfDependencies = testClasspath +++ testDependencies.all
  val sizeOfJar = testDependencies.libraries ** (jvmSizeOf.name + "*.jar")
  lazy val testPerf = task { args =>
    task {
      log.info("Running performance tests...")
      new Fork.ForkScala("com.codecommit.antixml.Performance")(None, Seq("-javaagent:" + sizeOfJar.absString, "-Xmx2g"), perfDependencies.getFiles, args, log) match {
        case 0 => None
        case x => Some("failed with error code " + x)
      }
    } dependsOn (testCompile, copyTestResources)
  }
}
