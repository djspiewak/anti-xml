package com.codecommit.antixml

import org.scalacheck.Prop._
import org.specs.{ ScalaCheck, Specification }

class BloomFilterSpec extends Specification {

  "Calling BloomFilter.apply" should {
    "throw an IllegalArgumentException for null elements" >> {
      BloomFilter(null: String) must throwA[IllegalArgumentException]
      BloomFilter(null: Seq[String]) must throwA[IllegalArgumentException]
    }
    "throw an IllegalArgumentException for a null factory" >> {
      BloomFilter("")(null) must throwA[IllegalArgumentException]
    }
  }
}

class DefaultBloomFilterSpec extends Specification with ScalaCheck {

  "Calling DefaultBloomFilter.contains" should {
    "throw an IllegalArgumentException for a null element" >> {
      new DefaultBloomFilter(Nil) contains null must throwA[IllegalArgumentException]
    }
    "never return true for elements that are not contained" >> {
      forAll { (s: String) =>
        (BloomFilter(Seq(s)) contains (s + "#")) == false
      } must pass
    }
  }

  "Calling DefaultBloomFilter.+" should {
    val filter = new DefaultBloomFilter(Seq("a"))
    "throw an IllegalArgumentException for a null element" >> {
      filter + null must throwA[IllegalArgumentException]
    }
    "return a new BloomFilter" >> {
      filter + "b" mustNotBe null
      filter + "1" mustNotBe null
    }
  }
}
