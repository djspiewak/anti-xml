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

import org.specs._
import org.scalacheck.Gen._
import org.scalacheck.Prop._

object CatamorphicVectorSpecs extends Specification with ScalaCheck {
  
  lazy val numProcessors = Runtime.getRuntime.availableProcessors
  implicit val params = set(workers -> numProcessors)
  
  def emptyVector[S, A](z: S) = CatamorphicVector[S, A](z) { (_: S) => None }
  
  def singletonVector[A](a: A): CatamorphicVector[Boolean, A] =
    CatamorphicVector(true) { (b: Boolean) => if (b) Some(false -> a) else None }
  
  def listToVector[A](xs: List[A]): CatamorphicVector[List[A], A] = {
    CatamorphicVector(xs) {
      case hd :: tail => Some(tail -> hd)
      case Nil => None
    }
  }

  "CatamorphicVector" >> {
    "apply should preserve the ordering of its elements" in {
      val naturals = CatamorphicVector(0) { (n: Int) => Some(n + 1 -> n) }
      
      choose(0, 10000) must pass { x: Int =>
        val (result, naturals2) = naturals(x)
        result mustEqual x
        
        val (result2, naturals3) = naturals2(x / 2)
        result2 mustEqual (x / 2)
        
        naturals3(x)._1 mustEqual x
      }
    }
    
    "updated should modify the specified index (and only that index)" in {
      val naturals = CatamorphicVector(0) { (n: Int) => Some(n + 1 -> n) }
      
      choose(0, 10000) must pass { x: Int =>
        var naturals2 = naturals.updated(x, -42)      // not a natural...
        
        // not actually comprehensive, but hopefully faster
        math.max(x - 100, 0) until (x + 100) forall { i =>
          val (result, naturals3) = naturals2(i)
          naturals2 = naturals3
          if (i == x)
            result mustEqual -42
          else
            result mustEqual i
        }
      }
    }
    
    "prepending to empty should have the same elements as the singleton CatamorphicVector" in {
      choose(0, 100000) must pass { x: Int =>
        (x +: emptyVector(0)).force mustEqual singletonVector(x).force
      }
    }
    
    "prepending should be equivalent to appending on empty" in {
      choose(0, 100000) must pass { x: Int =>
        (x +: emptyVector(0)).force mustEqual (emptyVector(0) :+ x).force
      }
    }
    
    "appending to empty should be equivalent to the singleton CatamorphicVector" in {
      choose(0, 100000) must pass { x: Int =>
        (emptyVector(0) :+ x).force mustEqual singletonVector(x).force
      }
    }
    
    "lazy ++ should be isomorphic to Vector ++" in {
      val prop = forAll { (left: List[Int], right: List[Int]) =>
        (listToVector(left) ++ listToVector(right)).force mustEqual Vector(left ++ right: _*)
      }
      
      prop must pass
    }
    
    "mapping id must equal the original" in {
      val prop = forAll { xs: List[Int] =>
        (listToVector(xs) map identity force) mustEqual listToVector(xs).force
      }
      
      prop must pass
    }

    "forcing evaluation of a mapping must be equivalent to a strict mapping" in {
      forAll { xs: List[Int] =>
        Vector(xs map (_ + 1): _*) mustEqual (listToVector(xs) map (_ + 1) force)
      } must pass
    }

    "composing maps should retain the order of application" in {
      forAll { xs: List[Int] =>
        val strict = Vector(xs map (_ + 1) map (_ * 2): _*)
        val nonStrict = (listToVector(xs) map (_ + 1) map (_ * 2) force)
        strict mustEqual nonStrict
      } must pass
    }

    "random-access to a mapped element must force evaluation of the mapping" in {
      forAll { s: String =>
        val cata = listToVector(s :: Nil)
        cata.map(_ + "!")(0)._1 mustEqual (cata(0)._1 + "!")
      } must pass
    }
    
    "map f . g should be equivalent to map f . map g" in {
      val f = { x: Int => x + 1 }
      val g = { x: Int => x * 2 }
        
      val prop = forAll { (xs: List[Int]) =>
        (listToVector(xs) map g map f force) mustEqual (listToVector(xs) map (f compose g) force)
      }
      
      prop must pass
    }
    
    "force on a singleton vector should return that value" in {
      val prop = forAll { x: Int =>
        singletonVector(x).force mustEqual Vector(x)
      }
      
      prop must pass
    }
    
    "force on a List-based vector should return the original List" in {
      val prop = forAll { xs: List[Int] =>
        listToVector(xs).force mustEqual Vector(xs: _*)
      }
      
      prop must pass
    }

    "length should force evaluation and generate accurate values" in {
      forAll { xs: List[String] =>
        listToVector(xs).length._1 mustEqual xs.length
      } must pass
    }
  }
}
