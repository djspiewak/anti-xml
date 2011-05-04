import sbt._

import de.element34.sbteclipsify._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify with ScctProject {

  val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test" withSources
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7.1" % "test" withSources
  val specs2 = "org.specs2" %% "specs2" % "1.3-SNAPSHOT" % "test" withSources
  val jvmSizeOf = "com.github.dmlap" %% "sizeof" % "0.1" % "test" from "http://cloud.github.com/downloads/dmlap/jvm-sizeof/jvm-sizeof-0.1.jar"

  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

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
  
  override def documentOptions =
    CompoundDocOption("-sourcepath", mainScalaSourcePath.asFile.getCanonicalPath) ::
    CompoundDocOption("-doc-source-url", "https://github.com/djspiewak/anti-xml/tree/master/src/main/scala€{FILE_PATH}.scala") ::
    super.documentOptions.toList
    
  override def managedStyle = ManagedStyle.Maven
  
  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")
  
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)
  
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)
  
  override def defaultPublishRepository = {
    val nexus = "http://nexus.scala-tools.org/content/repositories/"
    if (version.toString.endsWith("SNAPSHOT"))
      Some("scala-tools snapshots" at nexus + "snapshots/")
    else
      Some("scala-tools releases" at nexus + "releases/")
  }
  
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  
  val snapshots = "snapshots repository" at "http://scala-tools.org/repo-snapshots"
}
