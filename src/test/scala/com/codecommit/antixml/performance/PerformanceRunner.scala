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

package com.codecommit.antixml
package performance

class PerformanceRunner(trials: Seq[Trial]) {
  import PerformanceRunner._

  /** Runs the trials.
   */
  def run(args: Seq[String]) {
    if (args.contains("--help")) {
      println(usage)
      println("%-27s %-30s %s".format("id","classifiers","description"))
      println("%-27s %-30s %s".format("--","-----------","-----------"))
      for(t <- trials) {
        println("%-27s %-30s %s".format(
          t.id.name,
          t.classifiers.toSeq.map(_.name).sorted.mkString("(",", ",")"),
          t.description
        ))
      }
      println()
    } else {
      run0(args)
    }
  }
  
  private def run0(args: Seq[String]) {
    
    val (switches,selectors) = Vector(args:_*).partition({_.startsWith("-")})
    
    val trialSelector = parseTrialSelector(if (selectors.isEmpty) Seq("0") else selectors)
    
    println("-- System Information --")
    println("Heap: " + (Runtime.getRuntime.maxMemory / (1024 * 1024)) + "MB")
    println("Java: " + System.getProperty("java.vendor") + " " + System.getProperty("java.version"))
    println("OS: " + System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch"))
    println()

    val filtered = trials filter {trial: Trial => trialSelector(TrialCriertia(trial))}
    
    val filteredLoadTrials = filtered collect {case lt: LoadTrial => lt}
    
    for(loadTrial <- filteredLoadTrials) {
      println("-- Memory Usage ("+loadTrial.sizeDescription+") --")
      for(impl <- loadTrial.create.sizeMeasurements) {
        print("%-15s ".format(impl.description+":"))
        cleanVM()
        println(deepsize(impl.run()))
      }
      println()
    }
        
    if (!filtered.isEmpty) {
      val f = { () =>
        println("-- Execution Time --")
        filtered foreach { trial =>
          cleanVM()
          val (_, time) = timedMs(timeTrial(trial, trialSelector))
          if (switches.contains("--totaltime")) {
            println("   trial completed in "+time+" ms")
          }
          println()
        }
      }
      val iter = if (switches.contains("--loop")) Iterator.continually(f) else Iterator(f)
      iter foreach {_()}
    }
  }
  
  private def parseTrialSelector(termStrings: Seq[String]): TrialCriteria => Boolean = {
    import scala.util.control.Exception.catching
    
    object IntString {
      def unapply(s: String): Option[Int] = catching(classOf[NumberFormatException]) opt {Integer.parseInt(s)}
    }
    
    object Decompose {
      def unapply(s: String): Option[(Char, String)] = if (s.isEmpty) None else Some(s(0),s drop 1)
    }
    
    def toPred(arg: String): TrialCriteria => Boolean = arg match {
      case IntString(num) => _.runLevel <= num
      case Decompose('!',s) => trial => !toPred(s)(trial)
      case Decompose(':',s) => _.classifiers.contains(Symbol(s))
      case _ => _.id.name == arg
    }
    
    val terms = termStrings map { arg =>
      val preds = arg.split("\\&").map(toPred)
      trial: TrialCriteria => preds forall {p => p(trial)}
    }
    trial: TrialCriteria => terms exists {p => p(trial)}
  }
  

  private def timedMs[A](f: => A): (A,Long) = {
    val start = System.nanoTime
    val res:A = f
    val elapsed = (System.nanoTime - start) / 1000000
    (res,elapsed)
  }

  private def timeTrial(trial: Trial, selector: TrialCriteria => Boolean) {
    val trialInstance = trial.create
    println("%-27s %-50s [%s]".format("["+trial.id.name+"]", trial.description, trialInstance.testDataDescription))
    System.out.flush()
    cleanVM()
    val impls = trial.create.implementations filter { impl =>
      selector(TrialCriteria(impl.id, impl.runLevel, impl.classifiers))
    }
    impls foreach { impl =>
      print(" + %-27s ".format(impl.description+":"))
      System.out.flush()
      
      val warmUpResults = InfoResults((0 until impl.warmUps) map { _ => 
        val a = impl.preload()
        val r = impl.run(a)
        trialInstance.resultDescription(r)
      })
      if (!warmUpResults.isValid) 
        println("ERROR DURING WARMUP: "+warmUpResults)
      
      cleanVM()
      
      val results = (0 until impl.runs).map { _ =>
        val a = impl.preload()      // existential types for the win!
        cleanVM()
        val (r,t) = timedMs { impl.run(a) }
        (trialInstance.resultDescription(r), t)
      }
      
      val (infos,times) = results.unzip
      val timeReport = TimingResults(times).report
      val infoReport = InfoResults(infos).report
      println(" %-50s [%s]".format(timeReport,infoReport))
      System.out.flush()
    }
  }
  
}

object PerformanceRunner {
  private case class TrialCriteria(id: Symbol, runLevel: Int, classifiers: Set[Symbol])

  private object TrialCriertia {
    def apply(t: Trial) = TrialCriteria(t.id, t.runLevel, t.classifiers)
  }
  
  private case class InfoResults(data: Seq[String]) {
    def report: String =
      if (!isValid)
        "MISMATCHED RESULTS: " + List(data:_*)
      else
        data.headOption.getOrElse("")
    
    def isValid: Boolean = Set(data:_*).size <= 1
    
    override def toString = report
  }
  
  private class TimingResults private(val data: Seq[Long], val min: Long, val max: Long, val average: Long) {
    def report = "min:%4d ms, max:%4d ms, average:%4d ms".format(min,max,average) 

    override def toString = report
  }
  
  private object TimingResults {
    def apply(results: Seq[Long]): TimingResults = {
      val (min, max, sum) = results.foldLeft(java.lang.Long.MAX_VALUE, 0L, 0L) { case ((min, max, sum), result) =>
        (math.min(min, result), math.max(max, result), sum + result)
      }
      new TimingResults(results, min, max, sum / results.size)
    }
  }
  
  val usage = """
  |usage: [switch]... [selector]...
  |
  |A selector consists of one or more predicates separated by "&". Predicates are of the form:
  |  <id>          - matches any trial with an id of <id>
  |  :<clasifier>  - matches any trial containing <classifier> in its classifiers set
  |  any integer   - matches any trial of that runLevel or less
  |  !<predicate>  - matches any trial that does not match <predicate>
  |
  |A selector matches any trial that matches ALL of its predicates. The runner will
  |execute all trials matching ANY of the selectors on the command line.
  |
  |If no selectors are specified, a default selector of "0" is assummed  
  |
  |The following switches may be specified:
  |  --help        - prints this description
  |  --loop        - repeates the trials until the process is forcibly terminated
  |  --totaltime   - prints the total time to execute each trial, including setup and teardown
  |
  |The following trials may be run:
  |""".stripMargin
  
}
