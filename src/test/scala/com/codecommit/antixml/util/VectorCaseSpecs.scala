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

package com.codecommit.antixml.util

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.specs2.matcher.Parameters
import org.scalacheck.{Arbitrary, Prop}
import Prop._
import org.specs2.matcher.ScalaCheckMatchers._

class VectorCaseSpecs extends Specification with ScalaCheck {
  import math._
  
  val vector = VectorCase[Int]()
  
  "VectorCase" should {
    "store a single element" in {
      val v2 = vector :+ 42
      v2(0) mustEqual 42
    }
    
    "implement +:" in check { (vec: VectorCase[Int], i: Int) =>
      val vec2 = i +: vec
      vec2.length mustEqual (vec.length + 1)
      vec2.head mustEqual i
      vec.zipWithIndex forall {
        case (x, i) => vec2(i + 1) mustEqual x
      }
    }
    
    "implement :+" in check { (vec: VectorCase[Int], i: Int) =>
      val vec2 = vec :+ i
      vec2.length mustEqual (vec.length + 1)
      vec2.last mustEqual i
      vec.zipWithIndex forall {
        case (x, i) => vec2(i) mustEqual x
      }
    }
    
    "implement ++" in check { (vec1: VectorCase[Int], vec2: VectorCase[Int]) =>
      val result1 = vec1 ++ vec2
      val result2 = vec2 ++ vec1
      
      result1.length mustEqual (vec1.length + vec2.length)
      result1.length mustEqual result2.length
      
      vec1.zipWithIndex forall {
        case (x, i) => {
          result1(i) mustEqual x
          result2(i + vec2.length) mustEqual x
        }
      }
      
      vec2.zipWithIndex forall {
        case (x, i) => {
          result2(i) mustEqual x
          result1(i + vec1.length) mustEqual x
        }
      }
    }
    
    "implement length" in check { list: List[Int] =>
      val vec = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
      vec.length === list.length
    }
    
    "replace single element" in check { (vec: VectorCase[Int], i: Int) =>
      (vec.nonEmpty && i > Int.MinValue) ==> {
        val idx = abs(i) % vec.length
        val newVectorCase = vec.updated(idx, "test").updated(idx, "newTest")
        newVectorCase(idx) mustEqual "newTest"
      }
    }
    "fail on apply out-of-bounds" in check { (vec: VectorCase[Int], i: Int) =>
      !((0 until vec.length) contains i) ==> { vec(i) must throwA[Throwable] }
    }

    "fail on update out-of-bounds" in check { (vec: VectorCase[Int], i: Int) =>
      !((0 until vec.length) contains i) ==> { vec.updated(i, 42) must throwA[Throwable] }
    }
    
    "store multiple elements in order" in check { list: List[Int] =>
      val newVectorCase = list.foldLeft(vector) { _ :+ _ }
      val res = for (i <- 0 until list.length) yield newVectorCase(i) == list(i)

      res must not contain (false)
    }

    "store lots of elements" in {
      val LENGTH = 100000
      val vector = (0 until LENGTH).foldLeft(VectorCase[Int]()) { _ :+ _ }
      
      vector.length mustEqual LENGTH
      ((i:Int) => vector(i) mustEqual i).forall(0 until LENGTH)
    }
    
    "maintain both old and new versions after conj" in check { vec: VectorCase[Int] =>
      val vec2 = vec :+ 42
      for (i <- 0 until vec.length) {
        vec2(i) aka ("Index " + i + " in derivative") mustEqual vec(i) aka ("Index " + i + " in origin")
      }
      vec2.last mustEqual 42
    }.set(maxSize -> 3000, minTestsOk -> 1000, workers -> numProcessors)

    "maintain both old and new versions after update" in check { (vec: VectorCase[Int], i: Int) =>
      (!vec.isEmpty && i > Int.MinValue) ==> {
        val idx = abs(i) % vec.length
        val vec2 = vec.updated(idx, 42)
        for (i <- 0 until vec.length if i != idx) {
          vec2(i) aka ("Index " + i + " in derivative") mustEqual vec(i) aka ("Index " + i + " in origin")
        }
        vec2(idx) mustEqual 42
      }
    }
    
    "implement drop matching Vector semantics (for at least 4 base cases)" in check { vec: VectorCase[Int] =>
      for (len <- (vec.length - 4) to vec.length) {
        (vec drop len toVector) mustEqual (vec.toVector drop len)
      }
      true
    }
    
    "implement drop matching Vector semantics (in general case)" in check { (vec: VectorCase[Int], len: Int) =>
      (vec drop len toVector) mustEqual (vec.toVector drop len)
    }
    
    "implement dropRight matching Vector semantics (for at least 4 base cases)" in check { vec: VectorCase[Int] =>
      for (len <- (vec.length - 4) to vec.length) {
        (vec dropRight len toVector) mustEqual (vec.toVector dropRight len)
      }
      true
    }
    
    "implement dropRight matching Vector semantics (in general case)" in check { (vec: VectorCase[Int], len: Int) =>
      (vec dropRight len toVector) mustEqual (vec.toVector dropRight len)
    }
    
    "implement filter" in check { (vec: VectorCase[Int], f: (Int)=>Boolean) =>
      val filtered = vec filter f

      var back = filtered forall f
      for (e <- vec) {
        if (f(e)) {
          back &&= filtered.contains(e)
        }
      }
      back
    }
    
    "implement foldLeft" in check { list: List[Int] =>
      val vec = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
      vec.foldLeft(0) { _ + _ } === list.foldLeft(0) { _ + _ }
    }
    
    "implement foreach" in check { vec: VectorCase[Int] =>
      val b = Vector.newBuilder[Int]
      for(i <- vec)
        b += i
      val v = b.result()
      v mustEqual vec
    }
    
    "implement forall" in check { (vec: VectorCase[Int], f: (Int)=>Boolean) =>
      val bool = vec forall f
      
      var back = true
      for (e <- vec) {
        back &&= f(e)
      }
      
      (back && bool) || (!back && !bool)
    }
    
    "implement flatMap" in check { (vec: VectorCase[Int], f: (Int)=>VectorCase[Int]) =>
      val mapped = vec flatMap f
      
      var back = true
      
      var i = 0
      var n = 0
      
      while (i < vec.length) {
        val res = f(vec(i))
        
        var inner = 0
        while (inner < res.length) {
          back &&= mapped(n) == res(inner)
          
          inner += 1
          n += 1
        }
        
        i += 1
      }

      back
    }
    
    "implement init matching Vector semantics" in check { vec: VectorCase[Int] =>
      !vec.isEmpty ==> {
        vec.init.toVector mustEqual vec.toVector.init
      }
    }
    
    "implement map" in check { (vec: VectorCase[Int], f: (Int)=>Int) =>
      val mapped = vec map f
      
      var back = vec.length == mapped.length
      for (i <- 0 until vec.length) {
        back &&= mapped(i) == f(vec(i))
      }
      back
    }
    
    "implement reverse" in check { v: VectorCase[Int] =>
      val reversed = v.reverse
      
      var back = v.length == reversed.length
      for (i <- 0 until v.length) {
        back &&= reversed(i) == v(v.length - i - 1)
      }
      back
    }
    
    "append to reverse" in check { (v: VectorCase[Int], n: Int) =>
      val rev = v.reverse
      val add = rev :+ n
      
      var back = add.length == rev.length + 1
      for (i <- 0 until rev.length) {
        back &&= add(i) == rev(i)
      }
      back && add(rev.length) === n
    }
    
    "map on reverse" in check { (v: VectorCase[Int], f: (Int)=>Int) =>
      val rev = v.reverse
      val mapped = rev map f
      
      var back = mapped.length == rev.length
      for (i <- 0 until rev.length) {
        back &&= mapped(i) == f(rev(i))
      }
      back
    }
    
    /* "implement slice matching Vector semantics" in check { (vec: VectorCase[Int], from: Int, until: Int) =>
      // skip("Vector slice semantics are inconsistent with Traversable")
      vec.slice(from, until).toVector mustEqual vec.toVector.slice(from, until)
    } */
    
    "implement splitAt matching Vector semantics" in check { (vec: VectorCase[Int], i: Int) =>
      val (left, right) = vec splitAt i
      val (expectLeft, expectRight) = vec.toVector splitAt i
      
      left mustEqual expectLeft
      right mustEqual expectRight
    }
    
    "implement tail matching Vector semantics" in check { vec: VectorCase[Int] =>
      !vec.isEmpty ==> {
        vec.tail.toVector mustEqual vec.toVector.tail
      }
    }
    
    "implement take matching Vector semantics" in check { (vec: VectorCase[Int], len: Int) =>
      (vec take len toVector) mustEqual (vec.toVector take len)
    }
    
    "implement takeRight matching Vector semantics" in check { (vec: VectorCase[Int], len: Int) =>
      (vec takeRight len toVector) mustEqual (vec.toVector takeRight len)
    }
    
    "implement zip" in check { (first: VectorCase[Int], second: VectorCase[Double]) =>
      val zip = first zip second
      
      var back = zip.length == min(first.length, second.length)
      for (i <- 0 until zip.length) {
        var (left, right) = zip(i)
        back &&= (left == first(i) && right == second(i))
      }
      back
    }
    
    "implement zipWithIndex" in check { vec: VectorCase[Int] =>
      val zip = vec.zipWithIndex
      
      var back = zip.length == vec.length
      for (i <- 0 until zip.length) {
        val (elem, index) = zip(i)
        back &&= (index == i && elem == vec(i))
      }
      back
    }
    
    "implement equals" >> {
      "1." in check { list: List[Int] => 
        val vecA = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        val vecB = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        vecA === vecB
      }
      "2." in check { (vecA: VectorCase[Int], vecB: VectorCase[Int]) =>
        vecA.length != vecB.length ==> (vecA != vecB)
      }
      "3." in check { (listA: List[Int], listB: List[Int]) =>
        val vecA = listA.foldLeft(VectorCase[Int]()) { _ :+ _ }
        val vecB = listB.foldLeft(VectorCase[Int]()) { _ :+ _ }
        
        listA != listB ==> (vecA != vecB)
      }      
      "4." in check { (vec: VectorCase[Int], data: Int) => vec !== data }
    }
    
    "implement hashCode" in check { list: List[Int] =>
      val vecA = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
      val vecB = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
      vecA.hashCode === vecB.hashCode
    }
  }
  
  implicit def arbitraryVectorCase[A](implicit arb: Arbitrary[A]): Arbitrary[VectorCase[A]] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[A]]
    } yield data.foldLeft(VectorCase[A]()) { _ :+ _ })
  }
  
  val numProcessors = Runtime.getRuntime.availableProcessors
  implicit val params: Parameters = set(workers -> numProcessors)
}

  