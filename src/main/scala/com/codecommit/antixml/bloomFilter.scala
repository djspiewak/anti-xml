package com.codecommit.antixml

import scala.collection.immutable.BitSet
import scala.util.Random

object BloomFilter {
  import math._

  def apply(elements: Seq[Any] = Nil)(implicit conf: BloomFilterConfiguration): BloomFilter = {
    require(elements != null, "elements must not be null!")

//    val (m, k) = optimalMAndK(elements.size, conf.p)
    val (m, k) = (1024, 2)
    val hashes = elements flatMap hash(m, k)
    new BloomFilter(BitSet(hashes: _*), m, k)
  }

  private def hash(m: Int, k: Int)(element: Any): Seq[Int] = {
    // TODO Is tihs approach valid and if so does it offer enough performance?
    val rnd = new Random(0)
    val hashCode = element.hashCode
    (1 until k) map { _ => abs(hashCode ^ rnd.nextInt) % m }
  }

//  private def optimalMAndK(n: Int, p: Float): (Int, Int) = {
//    val m = {
//      val m = round((- n * log(p) / pow(log(2), 2)).toFloat)
//      if (m > 0) m else 1
//    }
//    val k =
//      if (n == 0) {
//        0
//      } else {
//        val k = round(log(2).toFloat * m / n)
//        if (k > 0) k else 1
//      }
//    (m, k)
//  }
}

private[antixml] class BloomFilter(private val bits: BitSet, m: Int, k: Int) {
  import BloomFilter._

  def contains(element: Any): Boolean =
    hash(m, k)(element) forall bits.contains

  def ++(that: BloomFilter): BloomFilter =
    new BloomFilter(this.bits union that.bits, m, k)
}

object BloomFilterConfiguration {

  // Values > 0.18f lead towards k == 2
  // Values < 0.25f tend to show more false trues than expected
  // 0.33f leads to significantly faster creation than 0.25f for 100000 elements
  implicit val default: BloomFilterConfiguration =
    BloomFilterConfiguration(0.33f)
}

case class BloomFilterConfiguration(p: Float) {
  require(p > 0, "p must be positive!")
  require(p < 1, "p must be less than 1!")
}
