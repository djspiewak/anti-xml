name := "anti-xml"

organization := "net.hamnaberg"

version := "0.4-SNAPSHOT"

crossScalaVersions := Seq("2.9.2", "2.9.1", "2.9.0-1", "2.9.0")

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

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
      val cp = depFiles ++ Seq(cdt,cdc)
      val vmArgs = Seq(
               "-javaagent:" + sizeOfJar.absString, 
               "-Xmx2g",
               "-XX:+UseSerialGC") //Seems to make System.gc() synchronous
      //To find bottlenecks, add "-agentlib:hprof=cpu=samples,depth=6" to vmArgs
      new Fork.ForkScala("com.codecommit.antixml.Performance")(None, vmArgs, cp, args, log) match {
        case 0 => None
        case x => Some("failed with error code " + x)
      }
  }  
}

doc in Compile <<= (clean in Compile, doc in Compile) map { (c, d) => d }

scalacOptions in Compile in doc <++= (unmanagedSourceDirectories in Compile) map { (usd) =>
  val scalaSrc: File = (usd filter { _.toString endsWith "scala" }).head
  Seq(
    "-sourcepath", scalaSrc.toString,
    "-doc-source-url", "https://github.com/hamnis/anti-xml/tree/master/src/main/scala€{FILE_PATH}.scala"
  )
}

publishArtifact in (Compile, packageBin) := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc) := true

publishArtifact in (Test, packageDoc) := false

publishArtifact in (Compile, packageSrc) := true

publishArtifact in (Test, packageSrc) := false

publishTo <<= (version) apply { (v: String) => 
  if (v.trim().endsWith("SNAPSHOT")) {
    Some("Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots") 
  } else {
    Some("Sonatype Nexus Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

//Maven central stuff
homepage := Some(new URL("http://anti-xml.org"))

startYear := Some(2011)

licenses := Seq(("BSD", new URL("https://github.com/hamnis/anti-xml/blob/master/LICENSE.rst")))

pomExtra <<= (pomExtra, name, description) {(pom, name, desc) => pom ++ xml.Group(
      <scm>
        <url>http://github.com/hamnis/anti-xml</url>
        <connection>scm:git:git://github.com/hamnis/anti-xml.git</connection>
        <developerConnection>scm:git:git@github.com:hamnis/anti-xml.git</developerConnection>
      </scm>
      <developers>
        <developer>
          <id>djspiewak</id>
          <name>Daniel Spiewak</name>
          <url>http://twitter.com/djspiewak</url>
        </developer>
      </developers>
      <contributors>
        <contributor>
          <name>Erlend Hamnaberg</name>
          <url>http://twitter.com/hamnis</url>
        </contributor>
      </contributors>
    )}
