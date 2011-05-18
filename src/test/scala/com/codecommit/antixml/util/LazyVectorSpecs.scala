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
import org.scalacheck._

class LazyVectorSpecs extends Specification with ScalaCheck {
  import Gen._
  import Prop._
  
  lazy val numProcessors = Runtime.getRuntime.availableProcessors
  implicit val params = set(workers -> numProcessors)
  
  def emptyVector[S, A](z: S) = LazyVector[S, A](z) { (_: S) => None }
  
  def singletonVector[A](a: A): LazyVector[Boolean, A] =
    LazyVector(true) { (b: Boolean) => if (b) Some(false -> a) else None }
  
  def listToVector[A](xs: List[A]): LazyVector[List[A], A] = {
    LazyVector(xs) {
      case hd :: tail => Some(tail -> hd)
      case Nil => None
    }
  }

  "LazyVector" >> {
    implicit val arbInt = Arbitrary(choose(0, 10000))
    
    "apply should preserve the ordering of its elements" in {
      val naturals = LazyVector(0) { (n: Int) => Some(n + 1 -> n) }
      
      check { x: Int =>
        val result = naturals(x)
        result mustEqual x
        
        val result2 = naturals(x / 2)
        result2 mustEqual (x / 2)
        
        naturals(x) mustEqual x
      }
    }
    
    "updated should modify the specified index (and only that index)" in {
      val naturals = LazyVector(0) { (n: Int) => Some(n + 1 -> n) }
      
      check { x: Int =>
        var naturals2 = naturals.updated(x, -42)      // not a natural...
        
        // not actually comprehensive, but hopefully faster
        math.max(x - 100, 0) until (x + 100) forall { i =>
          val result = naturals2(i)
          if (i == x)
            result mustEqual -42
          else
            result mustEqual i
        }
      }
    }
    
    "prepending to empty should have the same elements as the singleton LazyVector" in check { x: Int =>
      (x +: emptyVector(0)).force mustEqual singletonVector(x).force
    }
    
    "prepending should be equivalent to appending on empty" in check { x: Int =>
      (x +: emptyVector(0)).force mustEqual (emptyVector(0) :+ x).force
    }
    
    "appending to empty should be equivalent to the singleton LazyVector" in check { x: Int =>
      (emptyVector(0) :+ x).force mustEqual singletonVector(x).force
    }
    
    "lazy ++ should be isomorphic to Vector ++" in check { (left: List[Int], right: List[Int]) =>
      (listToVector(left) ++ listToVector(right)).force mustEqual Vector(left ++ right: _*)
    }
    
    "mapping id must equal the original" in check { xs: List[Int] =>
      (listToVector(xs) map identity force) mustEqual listToVector(xs).force
    }

    "forcing evaluation of a mapping must be equivalent to a strict mapping" in check { xs: List[Int] =>
      Vector(xs map (_ + 1): _*) mustEqual (listToVector(xs) map (_ + 1) force)
    }

    "composing maps should retain the order of application" in check { xs: List[Int] =>
      val strict = Vector(xs map (_ + 1) map (_ * 2): _*)
      val nonStrict = (listToVector(xs) map (_ + 1) map (_ * 2) force)
      strict mustEqual nonStrict
    }

    "random-access to a mapped element must force evaluation of the mapping" in check { s: String =>
      val cata = listToVector(s :: Nil)
      cata.map(_ + "!")(0) mustEqual (cata(0) + "!")
    }
    
    "map f . g should be equivalent to map f . map g" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x * 2 }
        
      check { (xs: List[Int]) =>
        (listToVector(xs) map g map f force) mustEqual (listToVector(xs) map (f compose g) force)
      }
    }
    
    "force on a singleton vector should return that value" in check { x: Int =>
      singletonVector(x).force mustEqual Vector(x)
    }
    
    "force on a List-based vector should return the original List" in check { xs: List[Int] =>
      listToVector(xs).force mustEqual Vector(xs: _*)
    }

    "length should force evaluation and generate accurate values" in check { xs: List[String] =>
      listToVector(xs).length mustEqual xs.length
    }
  }
}
