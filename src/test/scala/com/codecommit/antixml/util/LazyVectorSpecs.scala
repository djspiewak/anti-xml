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

object LazyVectorSpecs extends Specification with ScalaCheck {
   def emptyVector[S, A](z: S) = LazyVector[S, A](z)((_: S) => None)
  def listToVector[A](xs: List[A]): LazyVector[List[A], A] = {
   LazyVector(xs) {
      case x :: xs => Some(xs, x)
      case _ => None
    }
  }

  "LazyVector" should {
    "apply should preserve the ordering of its elements" in {
      def next(n: Int): Option[(Int, Int)] = Some(n + 1, n)
      val naturals = LazyVector(0)(next)
      choose(0, 100000) must pass { x: Int => naturals(x)._1 mustEqual x }
    }
    "updated should be pure" in {
      def one = LazyVector(1)((_: Int) => None)
      choose(0, 100000) must pass { x: Int =>
        one.updated(0, x) mustEqual one.updated(0, x)
      }
    }
    "prepending to empty should be equivalent to the singleton LazyVector" in {
      choose(0, 100000) must pass { x: Int =>
        x +: emptyVector(0) mustEqual LazyVector(x)((_: Int) => None)
      }
    }
    "prepending should be equivalent to appending on empty" in {
      choose(0, 100000) must pass { x: Int =>
        x +: emptyVector(0) mustEqual emptyVector(0) :+ x
      }
    }
    "appending to empty should be equivalent to the singleton LazyVector" in {
      choose(0, 100000) must pass { x: Int =>
        emptyVector(0) :+ x mustEqual LazyVector(x)((_: Int) => None)
      }
    }
    "lazy ++ should be isomorphic to List ++" in {
      forAll {(l: List[String], r: List[String]) =>
        (l ++ r) == ((listToVector(l) ++ listToVector(r)).force.toList)
      } must pass
    }
    "mapping id must equal the original" in {
      forAll { xs: List[String] =>
        listToVector(xs).map(x => x) mustEqual listToVector(xs)
       } must pass
    }
    "map f . g should be equivalent to map f . map g" in {
      forAll { xs: List[Int] =>
        def f = { x: Int =>  x + 1 }
        def g = { x: Int => x * 2 }
        listToVector(xs).map(g).map(f) mustEqual listToVector(xs).map(f andThen g)
      } must pass
    }
    "flatMap const empty should equal empty" in {
      forAll { xs: List[String] =>
        listToVector(xs) flatMap { _ => emptyVector(Nil) } mustEqual emptyVector(Nil)
      } must pass
    }
    "collect" in {
    }
    "force" in {
    }
  }
}
