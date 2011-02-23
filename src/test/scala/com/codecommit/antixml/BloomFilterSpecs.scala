package com.codecommit.antixml

import org.specs.Specification
import scala.util.Random

class BloomFilterSpecs extends Specification {

  "Calling BloomFilter.contains" should {
    val words =
      for {
        length <- 1 to 20
        i <- 1 to 5000
      } yield word(length)
    val time0 = System.currentTimeMillis
    val bloomFilter = BloomFilter(words)()
    val time1 = System.currentTimeMillis

    "never return a wrong false" >> {
      val falses = words filter { word => !(bloomFilter contains word) }
      falses must beEmpty
    }

//    "DUMMY" >> {
//      val otherWords =
//        for {
//          length <- 1 to 20
//          i <- 1 to 5000
//        } yield "b?r" + word(length) + "f##"
//      val trues = otherWords filter bloomFilter.contains
//      println("*** false trues: " + trues.size)
//      1 mustEqual 1
//    }
  }

  "Calling BloomFilter.++" should {
    "return a new BloomFilter" >> {
      val filter1 = BloomFilter(Seq("a"))()
      val filter2 = BloomFilter(Seq("b"))()
      filter1 ++ filter2 mustNotBe null
    }
  }

  private val rnd = new Random

  private val aToZ = 'a' to 'z'

  private def word(length: Int, s: String = ""): String =
    if (length < 1) {
      s
    } else {
      val c = aToZ(rnd nextInt aToZ.size)
      word(length - 1, s + c)
    }
}
