package com.codecommit.antixml

import org.specs._
import org.scalacheck._

object BloomFilterSpecs extends Specification with ScalaCheck {
  import Prop._
  import scala.math._
  
  "contains" should {
    "never give a false negative" in {
      val prop = forAll { xs: List[String] =>
        val filter = BloomFilter(xs)()
        xs mustNotExist { x => !filter.contains(x) }
      }
      
      prop must pass(set(maxSize -> 1000))
    }
  }

  "++" should {
    "throw an exception on mismatched filter size" in {
      val filter1 = BloomFilter(List('a, 'b, 'c))(n=6)
      val filter2 = BloomFilter(List('d, 'e))(n=4)
      
      (filter1 ++ filter2) must throwAn[IllegalArgumentException]
    }
    
    "return a BloomFilter which contains all of each" in {
      val prop = forAll { (xs1: List[String], xs2: List[String]) =>
        val width = (max(xs1.length, xs2.length) + 1) * 2
        val filter1 = BloomFilter(xs1)(n=width)
        val filter2 = BloomFilter(xs2)(n=width)
        val filter = filter1 ++ filter2
        
        xs1 mustNotExist { x => !filter.contains(x) }
        xs2 mustNotExist { x => !filter.contains(x) }
      }
      
      prop must pass(set(maxSize -> 1000))
    }
    
    "be commutative" in {
      val prop = forAll { (xs1: List[String], xs2: List[String]) =>
        val width = (max(xs1.length, xs2.length) + 1) * 2
        val filter1 = BloomFilter(xs1)(n=width)
        val filter2 = BloomFilter(xs2)(n=width)
        (filter1 ++ filter2) mustEqual (filter2 ++ filter1)
      }
      
      prop must pass(set(maxSize -> 1000))
    }
  }
}
