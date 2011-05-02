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
 * - Neither the name of the <ORGANIZATION> nor the names of its contributors may
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

package com.codecommit.antixml

import scala.io.Source
import com.github.dmlap.sizeof.SizeOf.deepsize
import javax.xml.parsers.DocumentBuilderFactory 

object Performance {
  def main(args: Array[String]) {
    import scala.util.control.Exception.catching
    val selectedTests: Trial => Boolean = args match {
      case Array() => _.runLevel == 0
      case args => {
        val symbols = Set((args map { Symbol(_) }): _*)
        (catching(classOf[NumberFormatException]) opt {
          val runLevel = Integer.parseInt(args(0))
          (_: Trial).runLevel <= runLevel
        }) getOrElse { trial: Trial =>
          symbols.contains(trial.id)
        }
      }
    }
    
    println("-- System Information --")
    println("Heap: " + (Runtime.getRuntime.maxMemory / (1024 * 1024)) + "MB")
    println("Java: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version"))
    println("OS: " + System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch"))
    println()
    
    if (selectedTests(LoadingXmlSmall)) {
      println("-- Memory Usage (7 MB) --")
      println("anti-xml:  " + deepsize(LoadingXmlSmall.antiXml.run(())))
      println("scala.xml: " + deepsize(LoadingXmlSmall.scalaXml.run(())))
      println("javax.xml: " + deepsize(LoadingXmlSmall.javaXml.run(())))
      println()
    }
    
    if (selectedTests(LoadingXmlLarge)) {
      println("-- Memory Usage (30 MB) --")
      println("anti-xml:  " + deepsize(LoadingXmlLarge.antiXml.run(())))
      println("scala.xml: " + deepsize(LoadingXmlLarge.scalaXml.run(())))
      println("javax.xml: " + deepsize(LoadingXmlLarge.javaXml.run(())))
      println()
    }
    
    val trials = List(LoadingXmlSmall,
      LoadingXmlLarge,
      ShallowSelectionSmall,
      DeepSelectionSmall,
      ShallowSelectionLarge,
      DeepSelectionLarge)
    
    val filtered = trials filter selectedTests
    if (!filtered.isEmpty) {
      println("-- Execution Time --")
      filtered foreach timeTrial
    }
  }

  def timedMs(f: =>Unit): Long = {
    val start = System.nanoTime
    f
    (System.nanoTime - start) / 1000000
  }

  def timeTrial(trial: Trial) {
    println(trial.description)
    trial.implementations foreach { impl =>
      print(" + " + impl.description + ": ")
      (0 until trial.warmUps) foreach { _ => impl.run(impl.preload()) }
      println(TimingResults((0 until trial.runs) map { _ =>
        System.gc()
        val a = impl.preload()      // existential types for the win!
        timedMs { impl.run(a) }
      }))
    }
  }

  case class Trial(id: Symbol, description: String) {
    var implementations: Seq[Implementation[_]] = Seq()
    val warmUps = 5
    val runs = 10
    val runLevel = 0

    object implemented {
      def by(desc: String) = new {
        def preload[A](a: =>A) = new {
          def in(impl: A => Any): Implementation[A] = {
            val result = new Implementation[A] {
              val description = desc
              def preload() = a
              def run(a: A) = {
                impl(a)
              }
            }
            implementations = implementations :+ result
            result
          }
        }
        def in(impl: =>Any): Implementation[Unit] = {
          val result = new Implementation[Unit] {
            val description = desc
            def preload() = ()
            def run(a: Unit) = {
              impl
            }
          }
          implementations = implementations :+ result
          result
        }
      }
    }
    
    trait Implementation[A] {
      type Value = A
      val description: String
      def preload(): Value
      def run(a: Value): Any
    }
  }

  object LoadingXmlSmall extends Trial('loadSmall, "Loading a 7 MB XML file") {
    val spendingPath = "/spending.xml"
    object SAXParser extends SAXParser
    val antiXml = implemented by "anti-xml" in {
      SAXParser.fromInputStream(getClass.getResourceAsStream(spendingPath))
    }
    object StAXParser extends StAXParser
    val antiXmlStAX = implemented by "anti-xml StAX" in {
      StAXParser.fromInputStream(getClass.getResourceAsStream(spendingPath))
    }
    val scalaXml = implemented by "scala.xml" in {
      scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    }
    val javaXml = implemented by "javax.xml" in {
      DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getResourceAsStream(spendingPath))
    }
  }

  object LoadingXmlLarge extends Trial('loadLarge, "Loading a 30 MB XML file") {
    override val runLevel = 2
    val spendingPath = "/discogs_20110201_labels.xml"
    
    object SAXParser extends SAXParser
    val antiXml = implemented by "anti-xml" in {
      SAXParser.fromInputStream(getClass.getResourceAsStream(spendingPath))
    }
    object StAXParser extends StAXParser
    val antiXmlStAX = implemented by "anti-xml StAX" in {
      StAXParser.fromInputStream(getClass.getResourceAsStream(spendingPath))
    }
    val scalaXml = implemented by "scala.xml" in {
      scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    }
    val javaXml = implemented by "javax.xml" in {
      DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getResourceAsStream(spendingPath))
    }
  }

  object ShallowSelectionSmall extends Trial('shallowSelectSmall, "Shallow selection in a 7 MB tree") {
    val spendingPath = "/spending.xml"
    
    lazy val antiTree = XML.fromInputStream(getClass.getResourceAsStream(spendingPath))
    lazy val scalaTree = scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    
    val antiXml = implemented by "anti-xml" in {
      antiTree \ "result" \ "doc" \ "MajorFundingAgency2"
      antiTree \ "foo" \ "bar" \ "MajorFundingAgency2"
    }
    val scalaXml = implemented by "scala.xml" in {
      scalaTree \ "result" \ "doc" \ "MajorFundingAgency2"
      scalaTree \ "foo" \ "bar" \ "MajorFundingAgency2"
    }
  }

  object DeepSelectionSmall extends Trial('deepSelectSmall, "Deep selection in a 7 MB tree") {
    val spendingPath = "/spending.xml"
    
    lazy val antiTree = XML.fromInputStream(getClass.getResourceAsStream(spendingPath))
    lazy val scalaTree = scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    lazy val domTree = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getResourceAsStream(spendingPath))
    
    val antiXml = implemented by "anti-xml" in {
      antiTree \\ "MajorFundingAgency2"
      antiTree \\ "fubar"
    }
    val scalaXml = implemented by "scala.xml" in {
      scalaTree \\ "MajorFundingAgency2"
      scalaTree \\ "fubar"
    }
    val javaXml = implemented by "javax.xml" in {
      {
        val result = domTree getElementsByTagName "MajorFundingAgency2"
        (0 until result.getLength) foreach (result item)
      }
      
      {
        val result = domTree getElementsByTagName "fubar"
        (0 until result.getLength) foreach (result item)
      }
    }
  }

  object ShallowSelectionLarge extends Trial('shallowSelectLarge, "Shallow selection in a 30 MB tree") {
    override val runLevel = 2
    val spendingPath = "/discogs_20110201_labels.xml"
    
    lazy val antiTree = XML.fromInputStream(getClass.getResourceAsStream(spendingPath))
    lazy val scalaTree = scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    
    val antiXml = implemented by "anti-xml" in {
      antiTree \ "label" \ "sublabels" \ "label"
      antiTree \ "foo" \ "bar" \ "label"
    }
    val scalaXml = implemented by "scala.xml" in {
      scalaTree \ "label" \ "sublabels" \ "label"
      scalaTree \ "foo" \ "bar" \ "label"
    }
  }

  object DeepSelectionLarge extends Trial('deepSelectLarge, "Deep selection in a 30 MB tree") {
    override val runLevel = 2
    val spendingPath = "/discogs_20110201_labels.xml"
    
    lazy val antiTree = XML.fromInputStream(getClass.getResourceAsStream(spendingPath))
    lazy val scalaTree = scala.xml.XML.load(getClass.getResourceAsStream(spendingPath))
    lazy val domTree = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getResourceAsStream(spendingPath))
    
    val antiXml = implemented by "anti-xml" in {
      antiTree \\ "sublabels"
      antiTree \\ "fubar"
    }
    val scalaXml = implemented by "scala.xml" in {
      scalaTree \\ "sublabels"
      scalaTree \\ "fubar"
    }
    val javaXml = implemented by "javax.xml" in {
      {
        val result = domTree getElementsByTagName "sublabels"
        (0 until result.getLength) foreach (result item)
      }
      
      {
        val result = domTree getElementsByTagName "fubar"
        (0 until result.getLength) foreach (result item)
      }
    }
  }

  class TimingResults private(val data: Seq[Long],
                              val min: Long,
                              val max: Long,
                              val average: Long) {
    override def toString = "min: " + min + " ms, max: " + max +
    " ms, average: " + average + " ms"
  }
  object TimingResults {
    def apply(results: Seq[Long]): TimingResults = {
      val (min, max, sum) = results.foldLeft(java.lang.Long.MAX_VALUE, 0L, 0L) { case ((min, max, sum), result) =>
        (math.min(min, result), math.max(max, result), sum + result)
      }
      new TimingResults(results, min, max, sum / results.size)
    }
  }
}
