package com.codecommit.antixml.util

import org.specs._
import org.scalacheck._

object VectorCaseSpecs extends Specification with ScalaCheck {
  import Prop._
  import math._
  
  val vector = VectorCase[Int]()
  
  implicit def arbitraryVectorCase[A](implicit arb: Arbitrary[A]): Arbitrary[VectorCase[A]] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[A]]
    } yield data.foldLeft(VectorCase[A]()) { _ :+ _ })
  }
  
  "VectorCase" should {
    "store a single element" in {
      val v2 = vector :+ 42
      v2(0) mustEqual 42
    }
    
    "implement length" in {
      val prop = forAll { (list: List[Int]) => 
        val vec = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        vec.length == list.length
      }
      
      prop must pass
    }
    
    "replace single element" in {
      val prop = forAll { (vec: VectorCase[Int], i: Int) =>
        (!vec.isEmpty && i > Int.MinValue) ==> {
          val idx = abs(i) % vec.length
          val newVectorCase = vec.updated(idx, "test").updated(idx, "newTest")
          newVectorCase(idx) == "newTest"
        }
      }
      
      prop must pass
    }
    
    "fail on apply out-of-bounds" in {
      val prop = forAll { (vec: VectorCase[Int], i: Int) =>
        !((0 until vec.length) contains i) ==> {
          vec(i) must throwA[Throwable]
        }
      }
      
      prop must pass
    }
    
    "fail on update out-of-bounds" in {
      val prop = forAll { (vec: VectorCase[Int], i: Int) =>
        !((0 to vec.length) contains i) ==> {
          vec.updated(i, 42) must throwA[Throwable]
        }
      }
      
      prop must pass
    }
    
    "store multiple elements in order" in {
      val prop = forAll { list: List[Int] =>
        val newVectorCase = list.foldLeft(vector) { _ :+ _ }
        val res = for (i <- 0 until list.length) yield newVectorCase(i) == list(i)
        
        res forall { _ == true }
      }
      
      prop must pass
    }
    
    "store lots of elements" in {
      val LENGTH = 100000
      val vector = (0 until LENGTH).foldLeft(VectorCase[Int]()) { _ :+ _ }
      
      vector.length mustEqual LENGTH
      for (i <- 0 until LENGTH) {
        vector(i) mustEqual i
      }
    }
    
    "maintain both old and new versions after conj" in {
      val prop = forAll { vec: VectorCase[Int] =>
        val vec2 = vec :+ 42
        for (i <- 0 until vec.length) {
          vec2(i) aka ("Index " + i + " in derivative") mustEqual vec(i) aka ("Index " + i + " in origin")
        }
        vec2.last mustEqual 42
      }
      
      prop must pass(set(maxSize -> 3000, minTestsOk -> 1000))
    }
    
    "maintain both old and new versions after update" in {
      val prop = forAll { (vec: VectorCase[Int], i: Int) =>
        (!vec.isEmpty && i > Int.MinValue) ==> {
          val idx = abs(i) % vec.length
          val vec2 = vec.updated(idx, 42)
          for (i <- 0 until vec.length if i != idx) {
            vec2(i) aka ("Index " + i + " in derivative") mustEqual vec(i) aka ("Index " + i + " in origin")
          }
          vec2(idx) mustEqual 42
        }
      }
      
      prop must pass(set(maxSize -> 3000, minTestsOk -> 1000))
    }
    
    "implement filter" in {
      val prop = forAll { (vec: VectorCase[Int], f: (Int)=>Boolean) =>
        val filtered = vec filter f
        
        var back = filtered forall f
        for (e <- vec) {
          if (f(e)) {
            back &&= filtered.contains(e)
          }
        }
        back
      }
      
      prop must pass
    }
    
    "implement foldLeft" in {
      val prop = forAll { list: List[Int] =>
        val vec = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        vec.foldLeft(0) { _ + _ } == list.foldLeft(0) { _ + _ }
      }
      
      prop must pass
    }
    
    "implement forall" in {
      val prop = forAll { (vec: VectorCase[Int], f: (Int)=>Boolean) =>
        val bool = vec forall f
        
        var back = true
        for (e <- vec) {
          back &&= f(e)
        }
        
        (back && bool) || (!back && !bool)
      }
      
      prop must pass
    }
    
    "implement flatMap" in {
      val prop = forAll { (vec: VectorCase[Int], f: (Int)=>VectorCase[Int]) =>
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
      
      prop must pass
    }
    
    "implement map" in {
      val prop = forAll { (vec: VectorCase[Int], f: (Int)=>Int) =>
        val mapped = vec map f
        
        var back = vec.length == mapped.length
        for (i <- 0 until vec.length) {
          back &&= mapped(i) == f(vec(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement reverse" in {
      val prop = forAll { v: VectorCase[Int] =>
        val reversed = v.reverse
        
        var back = v.length == reversed.length
        for (i <- 0 until v.length) {
          back &&= reversed(i) == v(v.length - i - 1)
        }
        back
      }
      
      prop must pass
    }
    
    "append to reverse" in {
      val prop = forAll { (v: VectorCase[Int], n: Int) =>
        val rev = v.reverse
        val add = rev :+ n
        
        var back = add.length == rev.length + 1
        for (i <- 0 until rev.length) {
          back &&= add(i) == rev(i)
        }
        back && add(rev.length) == n
      }
      
      prop must pass
    }
    
    "map on reverse" in {
      val prop = forAll { (v: VectorCase[Int], f: (Int)=>Int) =>
        val rev = v.reverse
        val mapped = rev map f
        
        var back = mapped.length == rev.length
        for (i <- 0 until rev.length) {
          back &&= mapped(i) == f(rev(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement zip" in {
      val prop = forAll { (first: VectorCase[Int], second: VectorCase[Double]) =>
        val zip = first zip second
        
        var back = zip.length == min(first.length, second.length)
        for (i <- 0 until zip.length) {
          var (left, right) = zip(i)
          back &&= (left == first(i) && right == second(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement zipWithIndex" in {
      val prop = forAll { vec: VectorCase[Int] =>
        val zip = vec.zipWithIndex
        
        var back = zip.length == vec.length
        for (i <- 0 until zip.length) {
          val (elem, index) = zip(i)
          
          back &&= (index == i && elem == vec(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement equals" in {
      {
        val prop = forAll { list: List[Int] => 
          val vecA = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
          val vecB = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
          
          vecA == vecB
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (vecA: VectorCase[Int], vecB: VectorCase[Int]) =>
          vecA.length != vecB.length ==> (vecA != vecB)
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (listA: List[Int], listB: List[Int]) =>
          val vecA = listA.foldLeft(VectorCase[Int]()) { _ :+ _ }
          val vecB = listB.foldLeft(VectorCase[Int]()) { _ :+ _ }
          
          listA != listB ==> (vecA != vecB)
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (vec: VectorCase[Int], data: Int) => vec != data }
        
        prop must pass
      }
    }
    
    "implement hashCode" in {
      val prop = forAll { list: List[Int] =>
        val vecA = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        val vecB = list.foldLeft(VectorCase[Int]()) { _ :+ _ }
        
        vecA.hashCode == vecB.hashCode
      }
      
      prop must pass
    }
  }
}

