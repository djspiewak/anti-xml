package com.codecommit.antixml

import scala.io.Source
import com.github.dmlap.sizeof.SizeOf.deepsize
import javax.xml.parsers.DocumentBuilderFactory 

object Performance {
  val tests: List[Test] = List(loadAntiSpending, loadScalaSpending, loadJavaSpending)
  val comparisons: List[(Test, Test)] = List(loadAntiSpending -> loadScalaSpending)

  def main(args: Array[String]) {
    println("Starting with " + (Runtime.getRuntime.maxMemory / (1024 * 1024)) + "MB")
    println("-- Memory Usage --")
    val memStart = System.nanoTime
    println("anti-xml: " + deepsize(antiSpending))
    println("scala.xml: " + deepsize(scalaSpending))
    println("javax.xml: " + deepsize(javaSpending))
    println("Memory Usage finished in " + durationMs(memStart) + " ms")
    println("-- Execution Time --")
    val execStart = System.nanoTime
    runTests()
    println()
    println("Execution Time finished in " + durationMs(execStart) + " ms")
    println("-- Comparisons --")
    val compStart = System.nanoTime
    runComparisons()
    println("Comparisons finished in " + durationMs(compStart) + " ms")
    println("Done")
  }

  @inline def durationMs(start: Long): Long =
    (System.nanoTime - start) / 1000000

  def runTests() {
    tests foreach { test =>
      println(test.description)
      println(runTest(test))
    }
  }

  def runComparisons() {
    comparisons foreach { case (left, right) =>
      println(left.description + " versus " + right.description)
      println(runTest(left) + "\t" + runTest(right))
    }
  }

  def runTest(test: Test): TimingResults = {
    // warm-up
    (0 until 5) foreach { _ => test.run() }
    TimingResults((0 until 10) map { _ =>
      System.gc()
      val start = System.nanoTime
      test.run()
      durationMs(start)
    })
  }

  trait Test {
    val description: String
    def run()
  }
  object Test {
    def apply(desc: String)(f: () => Unit) = new Test {
      val description = desc
      def run() {
        f()
      }
    }
  }

  class TimingResults private(val data: Seq[Long], val min: Long, val max: Long, val average: Long) {
    override def toString = "min: " + min + " ms, max: " + max + " ms, average: " + average + " ms"
  }
  object TimingResults {
    def apply(results: Seq[Long]): TimingResults = {
      val (min, max, sum) = results.foldLeft(java.lang.Long.MAX_VALUE, 0L, 0L) { case ((min, max, sum), result) =>
        (math.min(min, result), math.max(max, result), sum + result)
      }
      new TimingResults(results, min, max, sum / results.size)
    }
  }

  def loadDiscogs = Test("anti-xml loading a large file") { () =>
    XML.fromSource(Source.fromURL(getClass.getResource("/discogs_20110201_labels.xml")))
  }

  val spendingPath = "/spending.xml"

  def antiSpending =
    XML.fromSource(Source.fromURL(getClass.getResource(spendingPath)))
  def loadAntiSpending =
    Test("anti-xml loading a medium-sized XML file")(antiSpending _)

  def scalaSpending =
    scala.xml.XML.load(getClass.getResource(spendingPath))
  def loadScalaSpending =
    Test("scala.xml loading a medium-sized XML file")(scalaSpending _)

  def javaSpending =
    DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(getClass.getResourceAsStream(spendingPath))
  def loadJavaSpending =
    Test("javax.xml loading a medium-sized XML file")(javaSpending _)
    
}
