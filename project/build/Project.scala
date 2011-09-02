/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import sbt._

import de.element34.sbteclipsify._
import reaktor.scct.ScctProject

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify with ScctProject {

  val scalaCheck = {
    val scalacheckCrossVersion = crossScalaVersionString match {
      case "2.9.1" => "2.9.1.RC3"
      case v => v
    }
    val scalacheckVersion = crossScalaVersionString match {
      case "2.8.1" => "1.8"
      case _ => "1.9"
    }
    "org.scala-tools.testing" % ("scalacheck_" + scalacheckCrossVersion) % scalacheckVersion % "test" withSources
  }
  
  val specs2 = "org.specs2" %% "specs2" % "1.5" % "test" withSources
  val jvmSizeOf = "com.github.dmlap" %% "sizeof" % "0.1" % "test" from "http://cloud.github.com/downloads/dmlap/jvm-sizeof/jvm-sizeof-0.1.jar"

  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)
  
  val managedSourcePathRoot = path("src_managed")
  val managedSourcePath = managedSourcePathRoot / ("scala_" + buildScalaVersion)
  
  lazy val cleanManagedSource = task {
    def recursiveDelete(file: File) {
      if (file.isDirectory) {
        file.listFiles foreach recursiveDelete
      }
      file.delete()
    }
    
    if (managedSourcePath.exists) {
      log.info("Deleting directory " + managedSourcePath.absString)
      recursiveDelete(managedSourcePath.asFile)
    }
    None
  }
  
  // Scala compatibility hack
  lazy val generateCompatibilityTrait = task {
    log.info("Generating compatibility trait for Scala version " + buildScalaVersion)
    
    if (!managedSourcePathRoot.exists) {
      managedSourcePathRoot.asFile.mkdir()
    }
    
    if (!managedSourcePath.exists) {
      managedSourcePath.asFile.mkdir()
    }
    
    val body = if (buildScalaVersion startsWith "2.9") {
      """type CompatTraversable[A] = scala.collection.GenTraversableOnce[A]"""
    } else {
      """type CompatTraversable[A] = Traversable[A]"""
    }
    
    val fullSource = """
      package com.codecommit.antixml {
        private[antixml] trait ScalaCompat {""" + body + """}
      }
      """
    val outFile = managedSourcePath / "CompatTraversable.scala"
    
    val upToDate = if (outFile.exists)
      (Source fromFile outFile.asFile mkString) == fullSource
    else
      false
    
    if (!upToDate) {
      val writer = new BufferedWriter(new FileWriter(outFile.asFile))
      try {
        writer.append(fullSource)
      } finally {
        writer.close()
      }
    }
    None
  }
  
  override def compileAction = super.compileAction dependsOn generateCompatibilityTrait
  
  override def cleanAction = super.cleanAction dependsOn cleanManagedSource
  
  override def mainSourceRoots = super.mainSourceRoots +++ (managedSourcePath ##)

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
