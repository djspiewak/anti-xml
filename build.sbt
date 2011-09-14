name := "anti-xml"

organization := "com.codecommit"

version := "0.3-SNAPSHOT"

crossScalaVersions := Seq("2.9.1", "2.9.0-1", "2.9.0")

scalaVersion := "2.9.1"

libraryDependencies <++= (scalaVersion) { v =>
  Seq(
    "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources,
    "org.specs2" %% "specs2" % "1.5" % "test" withSources,
    "com.github.dmlap" %% "sizeof" % "0.1" % "test" from "http://cloud.github.com/downloads/dmlap/jvm-sizeof/jvm-sizeof-0.1.jar")
}


initialCommands in console := """import com.codecommit.antixml._
                                |val bookstore = <bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book><book><title>I, Robot</title><author>Isaac Asimov</author></book><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.convert
                                |val books = bookstore \ "book" """.stripMargin

InputKey[Option[String]]("test-perf") <<= inputTask { (args: TaskKey[Seq[String]]) =>
  (args, streams, classDirectory in Test, classDirectory in Compile, managedClasspath in Test,
   compile in Test, compile in Compile) map {
    (args, s, cdt, cdc, deps, ct, cc) =>
      val log = s.log
      log.info("Running performance tests...")
      val depFiles = deps map {_.data}
      val sizeOfJar = depFiles ** ("sizeof" + "*.jar")
      new Fork.ForkScala("com.codecommit.antixml.Performance")(None, 
          Seq("-javaagent:" + sizeOfJar.absString, "-Xmx2g"),
          depFiles ++ Seq(cdt, cdc), args, log) match {
        case 0 => None
        case x => Some("failed with error code " + x)
      }
  }  
}

doc in Compile <<= (clean in Compile, doc in Compile) map { (c, d) => d }

scaladocOptions in Compile <++= (unmanagedSourceDirectories in Compile) map { (usd) =>
  val scalaSrc: File = (usd filter { _.toString endsWith "scala" }).head
  Seq(
    "-sourcepath", scalaSrc.toString,
    "-doc-source-url", "https://github.com/djspiewak/anti-xml/tree/master/src/main/scala€{FILE_PATH}.scala"
  )
}

publishArtifact in (Compile, packageBin) := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc) := true

publishArtifact in (Test, packageDoc) := false

publishArtifact in (Compile, packageSrc) := true

publishArtifact in (Test, packageSrc) := false

publishTo <<= version { (v: String) =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if(v endsWith "-SNAPSHOT") Some("Scala Tools Nexus" at nexus + "snapshots/")
  else Some("Scala Tools Nexus" at nexus + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

resolvers += ScalaToolsSnapshots

