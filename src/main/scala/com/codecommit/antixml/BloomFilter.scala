package com.codecommit.antixml

import scala.collection.immutable.BitSet
import scala.util.Random

private[antixml] object BloomFilter {
  import math._

  def apply(elements: Seq[Any] = Nil)(n: Int = elements.size, p: Float = 0.33f): BloomFilter = {
    require(elements != null, "elements must not be null!")

    val (m, k) = optimalMAndK(n, p)
    val hashes = elements flatMap hash(m, k)
    new BloomFilter(BitSet(hashes: _*), n, m, k)
  }

  private def hash(m: Int, k: Int)(element: Any): Seq[Int] = {
    // TODO Is tihs approach valid and if so does it offer enough performance?
    val rnd = new Random(0)
    val hashCode = element.hashCode
    (1 until k) map { _ => abs(hashCode ^ rnd.nextInt) % m }
  }

  private def optimalMAndK(n: Int, p: Float): (Int, Int) = {
    val m = {
      val m = round((- n * log(p) / pow(log(2), 2)).toFloat)
      if (m > 0) m else 1
    }
    val k =
      if (n == 0) {
        1
      } else {
        val k = round(log(2).toFloat * m / n)
        if (k > 0) k else 1
      }
    (m, k)
  }
}

private[antixml] class BloomFilter(private val bits: BitSet, n: Int, m: Int, k: Int) {
  import BloomFilter._

  def contains(element: Any): Boolean =
    hash(m, k)(element) forall bits.contains

  def ++(that: BloomFilter): BloomFilter =
    new BloomFilter(this.bits union that.bits, n, m, k)
}
