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

package com.codecommit.antixml.util

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Parameters, Matcher}
import scala.math._

class BloomFilterSpecs extends Specification with ScalaCheck {

  "contains" should {
    "never give a false negative" in check { xs: List[String] =>
      val filter = BloomFilter(xs)()
      (filter must contain(_:Any)).forall(xs)
    }
  }

  "++" should {
    "throw an exception on mismatched filter size" in {
      val filter1 = BloomFilter(List('a, 'b, 'c))(n=6)
      val filter2 = BloomFilter(List('d, 'e))(n=4)
      
      (filter1 ++ filter2) must throwAn[IllegalArgumentException]
    }
    
    "return a BloomFilter which contains all of each" in check { (xs1: List[String], xs2: List[String]) =>
      val width = (max(xs1.length, xs2.length) + 1) * 2
      val filter1 = BloomFilter(xs1)(n=width)
      val filter2 = BloomFilter(xs2)(n=width)
      val filter = filter1 ++ filter2

      (filter must contain(_:Any)).forall(xs1)
      (filter must contain(_:Any)).forall(xs2)
    }
    
    "be commutative" in check { (xs1: List[String], xs2: List[String]) =>
      val width = (max(xs1.length, xs2.length) + 1) * 2
      val filter1 = BloomFilter(xs1)(n=width)
      val filter2 = BloomFilter(xs2)(n=width)
      (filter1 ++ filter2) mustEqual (filter2 ++ filter1)
    }
  }

  val numProcessors = Runtime.getRuntime.availableProcessors
  implicit val params: Parameters = set(maxSize -> (numProcessors * 1000), workers -> numProcessors)
  def contain(a: Any): Matcher[BloomFilter] = ((_:BloomFilter).contains(a), (_:BloomFilter).toString+" doesn't contain "+a)

}
