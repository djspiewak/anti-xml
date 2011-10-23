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

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.specs2.matcher.Parameters
import org.scalacheck.{Arbitrary, Prop}
import Prop._
import org.specs2.matcher.ScalaCheckMatchers._

class ZipperPathSpecs extends Specification with ScalaCheck {
  import math._
  
  //Shaemelessly stolen from VectorCaseSpecs
  
  val emptyPath = ZipperPath()
  
  "ZipperPath companion" should {

    "build using apply" in check { (items: List[Int]) =>
      val zp = ZipperPath(items:_*)
      items mustEqual List(zp:_*)
    }

    "build from a ZipperPath using fromSeq" in check { (items: ZipperPath) =>
      val zp = ZipperPath.fromSeq(items)
      List(items:_*) mustEqual List(zp:_*)
    }

    "build from an IndexedSeq using fromSeq" in check { (items: Vector[Int]) =>
      val zp = ZipperPath.fromSeq(items)
      List(items:_*) mustEqual List(zp:_*)
    }

    "build from a LinearSeq using fromSeq" in check { (items: List[Int]) =>
      val zp = ZipperPath.fromSeq(items)
      List(items:_*) mustEqual List(zp:_*)
    }

    "build from a ZipperPath using reversed" in check { (items: ZipperPath) =>
      val zp = ZipperPath.reversed(items)
      List(items:_*).reverse mustEqual List(zp:_*)
    }

    "build from an IndexedSeq using reversed" in check { (items: Vector[Int]) =>
      val zp = ZipperPath.reversed(items)
      List(items:_*).reverse mustEqual List(zp:_*)
    }

    "build from a LinearSeq using reversed" in check { (items: List[Int]) =>
      val zp = ZipperPath.reversed(items)
      List(items:_*).reverse mustEqual List(zp:_*)
    }
    
    "should have an empty empty" in {
      ZipperPath.empty.length mustEqual 0
    }

    "should produce builders" in check { (items: List[Int]) =>
      val zp = (ZipperPath.newBuilder ++= items).result
      List(items:_*) mustEqual List(zp:_*)
    }

  }
  
  "ZipperPath" should {
    "store a single element" in {
      val v2 = emptyPath :+ 42
      v2(0) mustEqual 42
    }
    
    "implement +:" in check { (zp: ZipperPath, i: Int) =>
      val zp2 = i +: zp
      zp2.length mustEqual (zp.length + 1)
      zp2.head mustEqual i
      zp.zipWithIndex forall {
        case (x, i) => zp2(i + 1) mustEqual x
      }
    }
    
    "implement :+" in check { (zp: ZipperPath, i: Int) =>
      val zp2 = zp :+ i
      zp2.length mustEqual (zp.length + 1)
      zp2.last mustEqual i
      zp.zipWithIndex forall {
        case (x, i) => zp2(i) mustEqual x
      }
    }
    
    "implement ++" in check { (zp1: ZipperPath, zp2: ZipperPath) =>
      val result1 = zp1 ++ zp2
      val result2 = zp2 ++ zp1
      
      result1.length mustEqual (zp1.length + zp2.length)
      result1.length mustEqual result2.length
      
      zp1.zipWithIndex forall {
        case (x, i) => {
          result1(i) mustEqual x
          result2(i + zp2.length) mustEqual x
        }
      }
      
      zp2.zipWithIndex forall {
        case (x, i) => {
          result2(i) mustEqual x
          result1(i + zp1.length) mustEqual x
        }
      }
    }
    
    "implement length" in check { list: List[Int] =>
      val zp = list.foldLeft(ZipperPath()) { _ :+ _ }
      zp.length === list.length
    }
    
    "replace single element" in check { (zp: ZipperPath, i: Int) =>
      (zp.nonEmpty && i > Int.MinValue) ==> {
        val idx = abs(i) % zp.length
        val newVectorCase = zp.updated(idx, "test").updated(idx, "newTest")
        newVectorCase(idx) mustEqual "newTest"
      }
    }
    "fail on apply out-of-bounds" in check { (zp: ZipperPath, i: Int) =>
      !((0 until zp.length) contains i) ==> { zp(i) must throwA[Throwable] }
    }
    
    "store multiple elements in order" in check { list: List[Int] =>
      val newVectorCase = list.foldLeft(emptyPath) { _ :+ _ }
      val res = for (i <- 0 until list.length) yield newVectorCase(i) == list(i)

      res must not contain (false)
    }

    "store lots of elements" in {
      val LENGTH = 100000
      val emptyPath = (0 until LENGTH).foldLeft(ZipperPath()) { _ :+ _ }
      
      emptyPath.length mustEqual LENGTH
      ((i:Int) => emptyPath(i) mustEqual i).forall(0 until LENGTH)
    }
    
    "maintain both old and new versions after conj" in check { zp: ZipperPath =>
      val zp2 = zp :+ 42
      for (i <- 0 until zp.length) {
        zp2(i) aka ("Index " + i + " in derivative") mustEqual zp(i) aka ("Index " + i + " in origin")
      }
      zp2.last mustEqual 42
    }.set(maxSize -> 3000, minTestsOk -> 1000, workers -> numProcessors)

    "maintain both old and new versions after update" in check { (zp: ZipperPath, i: Int) =>
      (!zp.isEmpty && i > Int.MinValue) ==> {
        val idx = abs(i) % zp.length
        val zp2 = zp.updated(idx, 42)
        for (i <- 0 until zp.length if i != idx) {
          zp2(i) aka ("Index " + i + " in derivative") mustEqual zp(i) aka ("Index " + i + " in origin")
        }
        zp2(idx) mustEqual 42
      }
    }
    
    "implement drop matching Vector semantics (in general case)" in check { (zp: ZipperPath, len: Int) =>
      toVector(zp drop len) mustEqual (toVector(zp) drop len)
    }
    
    "implement dropRight matching Vector semantics" in check { (zp: ZipperPath, len: Int) =>
      //IndexedSeqOptimized doesn't match Vector for negative lengths
      val l2 = max(0,len)
      toVector(zp dropRight l2) mustEqual (toVector(zp) dropRight l2)
    }
    
    "implement filter" in check { (zp: ZipperPath, f: (Int)=>Boolean) =>
      val filtered = zp filter f

      var back = filtered forall f
      for (e <- zp) {
        if (f(e)) {
          back &&= filtered.contains(e)
        }
      }
      back
    }
    
    "implement foldLeft" in check { list: List[Int] =>
      val zp = list.foldLeft(ZipperPath()) { _ :+ _ }
      zp.foldLeft(0) { _ + _ } === list.foldLeft(0) { _ + _ }
    }
    
    "implement foreach" in check { zp: ZipperPath =>
      val b = Vector.newBuilder[Int]
      for(i <- zp)
        b += i
      val v = b.result()
      v mustEqual zp
    }
    
    "implement forall" in check { (zp: ZipperPath, f: (Int)=>Boolean) =>
      val bool = zp forall f
      
      var back = true
      for (e <- zp) {
        back &&= f(e)
      }
      
      (back && bool) || (!back && !bool)
    }
    
    "implement flatMap" in check { (zp: ZipperPath, f: (Int)=>ZipperPath) =>
      val mapped = zp flatMap f
      
      var back = true
      
      var i = 0
      var n = 0
      
      while (i < zp.length) {
        val res = f(zp(i))
        
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
    
    "implement init matching Vector semantics" in check { zp: ZipperPath =>
      !zp.isEmpty ==> {
        toVector(zp.init) mustEqual toVector(zp).init
      }
    }
    
    "implement map" in check { (zp: ZipperPath, f: (Int)=>Int) =>
      val mapped = zp map f
      
      var back = zp.length == mapped.length
      for (i <- 0 until zp.length) {
        back &&= mapped(i) == f(zp(i))
      }
      back
    }
    
    "implement reverse" in check { v: ZipperPath =>
      val reversed = v.reverse
      
      var back = v.length == reversed.length
      for (i <- 0 until v.length) {
        back &&= reversed(i) == v(v.length - i - 1)
      }
      back
    }
    
    "append to reverse" in check { (v: ZipperPath, n: Int) =>
      val rev = v.reverse
      val add = rev :+ n
      
      var back = add.length == rev.length + 1
      for (i <- 0 until rev.length) {
        back &&= add(i) == rev(i)
      }
      back && add(rev.length) === n
    }
    
    "map on reverse" in check { (v: ZipperPath, f: (Int)=>Int) =>
      val rev = v.reverse
      val mapped = rev map f
      
      var back = mapped.length == rev.length
      for (i <- 0 until rev.length) {
        back &&= mapped(i) == f(rev(i))
      }
      back
    }
    
    /* "implement slice matching Vector semantics" in check { (zp: ZipperPath, from: Int, until: Int) =>
      // skip("Vector slice semantics are inconsistent with Traversable")
      zp.slice(from, until).toVector mustEqual zp.toVector.slice(from, until)
    } */
    
    "implement splitAt matching Vector semantics" in check { (zp: ZipperPath, i: Int) =>
      val (left, right) = zp splitAt i
      val (expectLeft, expectRight) = toVector(zp) splitAt i
      
      toVector(left) mustEqual expectLeft
      toVector(right) mustEqual expectRight
    }
    
    "implement tail matching Vector semantics" in check { zp: ZipperPath =>
      !zp.isEmpty ==> {
        toVector(zp.tail) mustEqual toVector(zp).tail
      }
    }
    
    "implement take matching Vector semantics" in check { (zp: ZipperPath, len: Int) =>
      toVector(zp take len) mustEqual (toVector(zp) take len)
    }
    
    "implement takeRight matching Vector semantics" in check { (zp: ZipperPath, len: Int) =>
      //scala.collection.IndexedSeqOptimized differs from Vector on negative lengths
      val l2 = max(0,len)
      toVector(zp takeRight l2) mustEqual (toVector(zp) takeRight l2)
    }
    
    "implement zip" in check { (first: ZipperPath, second: ZipperPath) =>
      val zip = first zip second
      
      var back = zip.length == min(first.length, second.length)
      for (i <- 0 until zip.length) {
        var (left, right) = zip(i)
        back &&= (left == first(i) && right == second(i))
      }
      back
    }
    
    "implement zipWithIndex" in check { zp: ZipperPath =>
      val zip = zp.zipWithIndex
      
      var back = zip.length == zp.length
      for (i <- 0 until zip.length) {
        val (elem, index) = zip(i)
        back &&= (index == i && elem == zp(i))
      }
      back
    }
    
    "implement equals" >> {
      "1." in check { list: List[Int] => 
        val zpA = list.foldLeft(ZipperPath()) { _ :+ _ }
        val zpB = list.foldLeft(ZipperPath()) { _ :+ _ }
        zpA === zpB
      }
      "2." in check { (zpA: ZipperPath, zpB: ZipperPath) =>
        zpA.length != zpB.length ==> (zpA != zpB)
      }
      "3." in check { (listA: List[Int], listB: List[Int]) =>
        val zpA = listA.foldLeft(ZipperPath()) { _ :+ _ }
        val zpB = listB.foldLeft(ZipperPath()) { _ :+ _ }
        
        listA != listB ==> (zpA != zpB)
      }      
      "4." in check { (zp: ZipperPath, data: Int) => zp !== data }
    }
    
    "implement hashCode" in check { list: List[Int] =>
      val zpA = list.foldLeft(ZipperPath()) { _ :+ _ }
      val zpB = list.foldLeft(ZipperPath()) { _ :+ _ }
      zpA.hashCode === zpB.hashCode
    }
  }
  
  implicit def arbitraryZipperPath: Arbitrary[ZipperPath] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[Int]]
    } yield ZipperPath.fromSeq(data))
  }
  implicit def arbitraryVector[A](implicit arb: Arbitrary[A]): Arbitrary[Vector[A]] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[A]]
    } yield Vector(data:_*))
  }

  def toVector(zp: ZipperPath): Vector[Int] = {
    (Vector[Int]() /: (0 until zp.length)) { (v,i) =>
      v :+ zp(i)
    }
  }
  
  val numProcessors = Runtime.getRuntime.availableProcessors
  implicit val params: Parameters = set(workers -> numProcessors)
}

  